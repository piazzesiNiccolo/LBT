1. implemented as subcommand of cargo, does not analyze dependencies for obvious efficiency reasons -> still vulnerable if dependency is not as thoroughly checked.
2. complicated handling of memory model, maybe integrating with separation logic?

3. reduction rules as conditional rewriting?

4. Unsound analysis for dynamic dispatch

5. Maybe too many dependencies? (Apron for for abs domains, Z3 csp
