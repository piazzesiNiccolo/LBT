
1. complicated and imprecise handling of memory model, maybe integrating with separation logic?
2. Unsound analysis for dynamic dispatch
3. C bindings for abstract domains, maybe internal domain library simpler and more maintainable choice?
4. nice feature being a cargo subcommand, no need for external workflow and integrates well with native rust toolchain.
5. using a theorem prover like Z3 vastly improves the classes of solvable problems and reuses well established tools without "reinventing the wheel".
6. The paper presents it as a bug detection tools, maybe using over-approximations  not the right choice? Could it be a better choice to under-approximate and tolerate some unsoundness but detecting correct bugs? (See limitations and future work, talk about Pulse and incorrectness logic.)
7. Refinement techniques instead of manually handling false positives?
8.  No dead store analysis because it's explicitly encoded in MIR-> nice!