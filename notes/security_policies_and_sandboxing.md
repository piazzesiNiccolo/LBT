# Security policies

We need to precisely define security policies and ways to actually enforce them.

E.G. CFI security policy: **control flow cannot be altered**

What is a policy in the mathematical sense? 
What is the programming abstraction to declare a policy?
What does it mean to enforce a policy?

What is the limit to what we can enforce?

We can start by defining **Execution monitors**


## Execution monitor

An execution monitor is an entity designed to control and limit the events raised by untrusted programs at runtime. Violation makes EMs intervene, possibly stopping execution.

The simplest for of execution monitor is OS level access control. For example an OS can decide if a process access a specific file by checking the access control list for that file.


EMs are runtime modules that run in parallel with an applications (similar to GCs) and may take decisions based on execution history.

**Intuition:** model program execution on a given input as a sequence of runtime events.


The sequence of runtime events form **traces**, which means we can use classical theory for model checking of kripke structure.

### Programs and policies

- An execution (trace) s is a sequence of security relevant events **e**.
- If a sequence is finite we simplify the formalism by adding an infinite repetition of $e_halt$. Now all traces are infinite.
- A program S is a set of traces 
- A policy P is a property of program -> subset of S ->  subset of the language defined by traces


Some examples:
1. Access control policies. Specify that no execution may access certain resources or invoke certain operations.
2. Availability policies. Specify that a program may acquire a resource but must release it at some arbitrary point
3. Bounded Availability policies. Same as 2 but the point of release is fixed
   

A property **P** denotes a language L(P) where the alphabet is the set of observable events.

$s \models P \leftrightarrow s \in L(P)$


Execution monitor enforce safety policies and are universally quantified predicates over executions.

P is called detector.

Since it enforces safety policies, the detector must be prefix-closed and all bad sequence are rejected in finite time, meaning there exists a prefix that gets rejected.


EM summary:


1. Analyze single current execution: 
   $P(\Pi) = (\forall \sigma \in \Pi: p(\sigma))$
2. Must truncate execution as soon as prefix violates policy 
   $\neg p(\tau) \implies (\forall \sigma: \neg p(\tau \sigma))$
3. Must detect violations in finite time
   $\neg p(\sigma) \implies (\exists i: \neg p(\sigma[..i]))$  

Security automate -> NFA automata to represent policies. Alphabet set of events, edge labels event predicates, all state accepting (prefix closed).


Traditional OS based EMs are programs that at runtime stops execution when it raises security event. 

**issues**
1. Inefficient because we do a context switch at every event 
2. Em extends the trusted computing base making it very large 
3. Wak since we can only see external program actions
4. non-modular: changing policy means changing the internals of OS


**Improvement:** in-line reference monitor

The idea is to implement an execution monitor and in-lining its logic into the target program. Can automate in-lining procedure, it's a program transformation.
CFI-enforcement: compiler autogenerate em code to instrument target program

**Challenges:**
- Autogenerate code 
- Preserve program logic
- Prevent em corruption
  
In-lining algorithm:
1. In.line automata before every event 
2. Partially evaluate the automaton edges to the event it guards (some disappear)
3. Generate guard code for remaining automaton logic


## Enforcement strategies

Take 1 : **Static Analysis**
Analyze untrusted code before running

Pros:
- immediate answer 
- no runtime overhead

Cons: 
- High load overhead 
- Weak in power...?

Recursively decidable policies 

Take 2: **EMs**

Em are external programs that monitor security events and intervene to prevent violation

Cons:
- No answer until execution 
- Runtime slow down (context switches)

Pros: 
- lower load time overhead, maybe more powerful? 

co-recursively enumerable policies


IRM 

Take untrusted program code and transform it adding  the inlined reference monitor  in a finite time. Transformed code must satisfy a specified policy and the behavior of code must be preserved. The main advantages are that runtime overhead AND load-time overhead are minimal.


# Sandboxes and access control in programming languages


One technique used in many levels of security is **compartmentalization**, which consists in separating and isolating parts of a system to decouple them and better enforce safety and security between them. 

The property of **isolation** can be broken down into a combination of confidentiality and integrity od data and code. The general way to enforce isolation is through sandboxes, which can be seen as an isolated environment where to execute untrusted process to protect a trusted one. Multiple sandboxes can coexist and there can be a hierarchy of compartments.
(VMs, containers,...)


**Issues**

1. Defining the TCB. A sandbox reduces the tcb of the executed functionality, BUT increases the tcb with the enforcement mechanism (What happens if it is compromised? privilege escalation etc..)
2. Can we control compartments with specific policies? How expressive and complex are these policies and can we handle them?
3. Need a defined, small and simple interface to interact with environment.

A compartment need a way to provide configurable access control mechanisms.

**Classical OS Access control**

Modern OS provides various form of compartmentalization and access control (processes, user and kernel space, access control rights,...) but they can be too complex depending on the context. Size of OS TCB is huge and has very complex interfaces. There is also no way to handle access control policies within a process, as processes are the computing units inside an operating system). Operating system are big programs that needs to handle a lot of functionalities and security issues, in amny cases it's a wrong choice to trust its mechanisms to define, configure, and enforce certain security policies.

A better solution would be to have **access control primitives inside programming languages**


Java stack inspection -> code-based access control.

Permissions, protections domains and policies as language entities.

Hardware sandboxes -> enclaves


Capability machines...