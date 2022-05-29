1. does not analyze dependencies for obvious efficiency reasons -> still vulnerable if dependency is not as thoroughly checked. (unsafe code is usually wrapped in safe interfaces and exposed through external libraries -> maybe check well known "unsafe" libraries?)
2. complicated handling of memory model, maybe integrating with separation logic?
3. reduction rules as conditional rewriting?
4. Unsound analysis for dynamic dispatch
5. C bindings for abstract domains, maybe internal domain library simpler and more maintainable choice?
6. nice feature being a cargo subcommand, no need for external workflow and integrates well with native rust toolchain.
7. using a theorem prover like Z3 vastly improves the classes of solvable problems and reuses well established tools without "reinventing the wheel".
8. The paper presents it as a bug detection tools, maybe using over-approximations  not the right choice? Could it be a better choice to under-approximate and tolerate some unsoundness but detecting correct bugs? (See limitations and future work, talk about Pulse and incorrectness logic.)
9. Refinement techniques instead of manually handling false positives?
10. No dead store analysis because it's explicitly encoded in the language-> nice!