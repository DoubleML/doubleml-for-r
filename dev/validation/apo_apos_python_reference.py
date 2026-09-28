"""Consume common.json from apo_apos_python_parity.R; never generate input data.

Regenerate with the R driver (its header documents dependencies and schema).
Run separately: python3 dev/validation/apo_apos_python_reference.py OUTPUT_DIR
Results are real Python DoubleML fits with externally supplied nuisance values.
Exit 2 denotes an import dependency failure only; mismatches raise and exit 1.
"""
import json
import math
from pathlib import Path
import sys


def compare_results(actual, expected, atol, rtol):
    """Fail on shapes, missing entries, nonfinite values or tolerance violations."""
    for scenario, models in expected.items():
        for model, metrics in models.items():
            for metric, target in metrics.items():
                value = actual[scenario][model][metric]
                key = f"{scenario}/{model}/{metric}"
                if value["shape"] != target["shape"]:
                    raise AssertionError(f"Shape mismatch: {key}")
                observed, wanted = value["values"], target["values"]
                if len(observed) != len(wanted) or len(wanted) != math.prod(target["shape"]):
                    raise AssertionError(f"Length mismatch: {key}")
                if any(not math.isfinite(x) or not math.isfinite(y) or
                       abs(x - y) > atol + rtol * abs(y)
                       for x, y in zip(observed, wanted)):
                    raise AssertionError(f"Numerical parity mismatch: {key}")
                print(f"PASS Python {key}")


def main():
    if len(sys.argv) != 2:
        raise ValueError("Supply the directory containing common.json and r_results.json.")
    output = Path(sys.argv[1])
    print(f"Python: {sys.version}", flush=True)
    print(f"Python interpreter: {sys.executable}", flush=True)
    try:
        import doubleml as dml
        import numpy as np
        import pandas as pd
        from doubleml.irm import DoubleMLAPO, DoubleMLAPOS
        from sklearn.dummy import DummyClassifier, DummyRegressor
    except ImportError as exc:
        print(f"DEPENDENCY UNAVAILABLE: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 2
    print(f"Python DoubleML: {dml.__version__}", flush=True)

    with (output / "common.json").open() as handle:
        common = json.load(handle)
    with (output / "r_results.json").open() as handle:
        expected = json.load(handle)
    print(f"R DoubleML: {common['r_doubleml_version']}", flush=True)
    print(f"Shared seed: {common['seed']}; folds: {common['n_folds']}; "
          f"repetitions: {common['n_rep']}", flush=True)
    levels = common["levels"]
    data = dml.DoubleMLData(pd.DataFrame(common["data"]), "y", "d", x_cols=["x"])
    splits = [[(np.asarray(f["train_ids"], dtype=int), np.asarray(f["test_ids"], dtype=int))
               for f in common["folds"]]]
    predictions = {}
    for level in levels:
        supplied = common["predictions"][str(level)]
        predictions[level] = {
            "ml_g_d_lvl1": np.asarray(supplied["ml_g"], dtype=float).reshape(-1, 1),
            "ml_g_d_lvl0": np.asarray(supplied["ml_g_d_lvl0"], dtype=float).reshape(-1, 1),
            "ml_m": np.asarray(supplied["ml_m"], dtype=float).reshape(-1, 1),
        }

    def pack(value):
        value = np.asarray(value, dtype=float)
        return {"shape": list(value.shape), "values": value.ravel(order="F").tolist()}

    def capture_framework(framework):
        # Python stores raw psi / mean(psi_deriv). APO's derivative is -1;
        # negate to compare the same centered-score convention as the R API.
        psi = -framework.scaled_psi
        # No public covariance property in Python: use its actual influence values.
        n_obs = psi.shape[0]
        covariance = np.stack(
            [psi[:, :, r].T @ psi[:, :, r] / n_obs**2 for r in range(psi.shape[2])],
            axis=2,
        )
        return {key: pack(value) for key, value in {
            "coef": framework.thetas, "se": framework.ses,
            "all_coef": framework.all_thetas, "all_se": framework.all_ses,
            "psi": psi, "covariance": covariance,
        }.items()}

    results = {}
    for normalize in (False, True):
        scenario = "normalized" if normalize else "unnormalized"
        kwargs = dict(
            obj_dml_data=data, ml_g=DummyRegressor(), ml_m=DummyClassifier(),
            n_folds=common["n_folds"], n_rep=common["n_rep"],
            normalize_ipw=normalize, trimming_threshold=common["trimming_threshold"],
            draw_sample_splitting=False,
        )
        obj = DoubleMLAPOS(treatment_levels=levels, **kwargs)
        obj.set_sample_splitting(splits)
        obj.fit(external_predictions=predictions)
        if list(obj.treatment_levels) != levels:
            raise AssertionError("Python changed the requested treatment order.")
        apo = DoubleMLAPO(treatment_level=common["reference"], **kwargs)
        apo.set_sample_splitting(splits)
        apo.fit(external_predictions={"d": predictions[common["reference"]]})
        contrast = obj.causal_contrast(reference_levels=common["reference"])
        results[scenario] = {
            "apo": {key: pack(value) for key, value in {
                "coef": apo.coef, "se": apo.se, "psi": apo.psi,
            }.items()},
            "apos": capture_framework(obj.framework),
            "contrast": capture_framework(contrast),
        }

    # Preserve genuine output even when comparison fails, for diagnosing differences.
    with (output / "python_results.json").open("w") as handle:
        json.dump(results, handle, indent=2, allow_nan=False)
    with (output / "python_versions.json").open("w") as handle:
        json.dump({"python": sys.version, "interpreter": sys.executable,
                   "doubleml": dml.__version__, "numpy": np.__version__,
                   "pandas": pd.__version__}, handle, indent=2)
    compare_results(results, expected, common["atol"], common["rtol"])
    return 0


if __name__ == "__main__":
    sys.exit(main())
