# Control Flow Integrity 

**Motivating example**

```c
1 #include <stdio.h>
2 #include <string.h>
3
4 #define AUTHMAX 4
5
6 struct auth {
7 char pass[AUTHMAX];
8 void (*func)(struct auth*);
9 };
10
11 void success() {
12 printf("Authenticated successfully\n");
13 }
14
15 void failure() {
16 printf("Authentication failed\n");
17 }
18
19 void auth(struct auth *a) {
20 if (strcmp(a->pass, "pass") == 0)
21 a->func = &success;
22 else
23 a->func = &failure;
24
25 }
26
27 int main(int argc, char **argv) {
28 struct auth a;
29
30 a.func = &auth;
31
32 printf("Enter your password:\n");
33 scanf("%s", &a.pass);
34
35 a.func(&a);
36 }
```

Password is read from stdin via a scanf() call. 
The issue in this case is the lack of input sanitation, which allows one to overwrite the function pointer. 

An attacker can exploit this weakness by overflowing the input and overwriting a.pass.  

**Control flow is controlled by the attacker when the func pointer is dereferenced.**



## WHAT IS CFI? 

Control Flow Integrity is a class of defenses aimed at mitigating and preventing control flow based attacks. Control flow attacks exploit input-dependent flow in a target program to redirect it to a controlled memory location, possibly leading to arbitrary code execution. As the example shows, lack of input control may lead to buffer overflows and more in general memory corruption, which in memory unsafe languages can lead to the overwrite of valid function address, hence leading to unexpected control flow.

THe goal of CFI is ensuring that the control flow is exactly as specified by code's flow. It does so by checking that every call and indirect branch lead to a valid function entry point or branch target.

### Implementation 

Normal runtime mechanisms for memory corruption helps but do not suffice (stack canaries,...). The approach followed in CFI is made of three parts:

- Define "expected behavior" statically

- Efficiently detect deviations from expectations at runtime 
- Avoid compromise of the detector, which is part of the TCB.
  

The "expected behavior" is defined by computing a **Control Flow Graph**. A control flow graph (CFG) is a graph used to represent  the flow of a programs. Nodes represent statements or, more in general, **basic blocks** of code, which are  a sequence of consecutive statements that must be executed in a sequential order, meaning that there are no jump or branching. Edges  represent control flow: how the execution traverse the program and gets redirected by branches and jumps.

Building   a CFG is pretty straight-forward: traverse the program (ast) and create basic blocks from related statements that do not jump or branch. Add edges from node i to node j if the last statement of block i branches to the first statement of node j. The challenging part is defining  the control flow for whole programs which require handling function call and returns. THe idea is to build a cfg in isolation for each function and then glue them together to reflect calls and returns. 

A call node  ```x = f(a,b,...,c)``` is split into two nodes 

$\square$ = f(a,b,...,c) $\rightarrow x = \square$

A return node ```return E``` is turned to an assignment ```res = E``` with res fresh. THen we add "call" and "return" edges

$\square = f(a,b,...,c) \rightarrow \text{start of f}$

$res = E \rightarrow x = \square$

**CFI enforcement (Abadi et.al) abstract algorithm**


1. For each control transfer, determine statically its possible destinations.
2. Insert a unique bit pattern (label) at every destination
3. **Instrumentation:** Insert binary code that at runtime will check wether the bit pattern of the target instruction matches one possible destination


**Algorithm steps**

    Hypothesis: 
       1. Immutable code
       2. Target address cannot change 

1. Compute the cfg in advance 
2. Monitor the control flow 
3. Monitor only indirect calls (JMP, returns with non constant target, call)


## Discussion and limitations 

How can we defeat CFI?

- Inject code with legal label $\longrightarrow$ Does not work if we assume non executable data 
- Modify code labels $\longrightarrow$ Does not work if we assume the code is immutable 
- Modify stack $\longrightarrow$ Does not work, adversary cannot chang registers where we load relevant data.


Good: CFI defeats control flow modifying attacks (Remote code injection, ROP,...)

BAD:
- mimicry attacks where we manipulate control flow only to allowed labels defeats the simple single label cfg.
- CFI does not work with dynamic libraries 
- CFI not compositional, a refactor requires recomputing the overall cfg.