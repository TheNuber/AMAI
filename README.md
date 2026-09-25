# AMAI — Algebraic Modelling of AI

**AMAI** is a lightweight OCaml DSL for building *symbolic performance models* of distributed AI/ML workloads. It features an embedded DSL designed to express specifications of performance behaviour, and map them to symbolic expressions for **latency**, **input/output size**, and **per-device memory** consumption.

This repository accompanies the paper:

> **AMAI: Algebraic Modelling of Distributed AI Computations for Symbolic Performance Analysis**  
> Rubén Coll Sánchez, Thibaut Tachon, Pierre Leca, Teng Su and Chong Li
> *PMBS 2026 workshop at SC 2026*

---


## Project Structure

```
AMAI/
├── bin/
│   ├── dune              
│   └── main.ml           
├── lib/
│   ├── dune                            # AMAI library definition
│   ├── expr.ml / expr.mli              # Symbolic expressions (add, mul, log, var, eval, …)
│   ├── tensor.ml / tensor.mli          # Small tensor library (shapes, zip, split, merge, …)
│   ├── dsl.ml / dsl.mli                # DSL (>>>, |||, par, reduce, extend, …)
│   ├── serialize.ml / serialize.mli    # CSV I/O: env templates, structure dumps, evaluation
├── test/
│   ├── example_inference/              # Minimal test A
│   └── training_step_qwen35_08B/       # Minimal test B
├── dune-project
└── README.md
```

---

## Building

AMAI requires OCaml, Dune, and `base`:

```bash
# Install dependencies (if not already present)
opam install dune

# Build everything
cd /path/to/AMAI
dune build
```

This compiles:
- The `AMAI` library
- The `main` CLI tool
- The test executables

---

## Command-Line Test Execution

Every test can be executed in the following way:

1. cd FOLDER
2. dune exec FOLDER generate   (*generates env template with variable values*)
3. dune exec FOLDER analyze    (*reads the template to output the final cost*)

---


