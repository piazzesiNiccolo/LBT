# Programming languages security issues

Unsecure software is everywhere, and programming languages are one of the major contributors to developing unsafe programs.

1. Misleading and confusing syntax constructs could lead to "clever" code that actually creates more issues than what it solve (think of c assignment expressions)

2. Weak or no typing could lead to running code that makes no sense, leading to unexpected crashes or worse, Undefined Behavior.

3. General lack of safety: memory unsafe languages (c, c++,...) are the major culprits but this is valid also for unsafe multithreading runtimes or unsound typecheckers leading to exploitable security vulnerabilities.  

**Issues**: how can an attacker exploit programming language design?

Running unsecure code can break all the relevant cia abstractions. Think of what can happen to data integrity with buffer overflows... but it can also cause service denial or hide malware silently by exploiting overflow attacks to do arbitrary code execution.

Threat model: attacker controls program inputs & execution environments.  Fully controls I/O, input files etc. and can partially control environment variables and other parts of os such as processes or threads. It has several ways of influencing code execution:
- take unexpected branches 
- change address of called function or exception handler
- read/write unexpected memory and more in general, unexpectedly access resources.
  

To analyze language vulnerabilities one must have a good understanding of the language model itself. Memory model used, type system, know operational semantics of the language itself (its abstract machine). For example, java or python gets compiled to a specific bytecode which is then executed by an abstract stack machine. KNowing how this all works could lead to develop possible exploits for programs written in these languages, possibly exploiting some abstractions that gets wrongfully translated from the high level language to the bytecode itself (think inner classes in java).


**Syntax issues**
bad syntactic choices may cause confusion in programmers and code reviewers about code behavior, leading to vulnerabilities.

```c
if((options==(_WCLONE | __WALL)) && (current->uid=0))
    retval = -EINVAL;
```

The single equals means that we are assigning the val 0 to uid instead of checking it to ==.
Unwanted side effect: uid becomes root, assignment evaluates to 0 which is logically false and the retval is never put to einval.

First solution: type systems 
BUt systems are usually incomplete, meaning that the reject valid programs.

Also weak type systems may create more problems than no type system at all (PHP, JavaScript) -> FALSE SENSE OF SECURITY.

Type coercion, compiler dependent behavior etc..

```java
short x = Short.MAX_VALUE;
System.out.println(x+1); 
```


```java
short x = Short.MAX_VALUE;
short y = (x+1); 
```

The first code runs fine, java coerce x to be a integer. IN the second case an exception is thrown.


**Semantics issues**

The majority of problems stems from wrongly specified semantics. UNdefined behaviors represent a large part of problems in the security context.

Aggressive optimizations conflicts with underspecified semantics, think for example of dead code eliminations with security checks for overflow.

Does using safe languages (functional, certificated compilers, rust,...) solve safety issues? In many cases yes but a few issues still arise:
- Usually these languages have some unsafe features, necessary to interact with other languages and operating systems interfaces(syscalls, ABIs for dynamic libraries etc..., FFI). In many cases this breaks the safety promises made by the language itself so it is a discouraged feature hidden behind explicit and complex APIs( unsafePerformIo in Haskell, unsafe Rust,...) but this is still necessary as an unfortunate consequences of the pervasiveness of unsafe languages in low level programming and operating systems
- High level safe languages usually comes with a big runtime, this may make them unfeasible in resource constrained situations such as embedded programming
- Safe code DOES NOT  mean secure code. Can still have insecure programs that  for example leak infos through information flow side channels.


## Memory corruption


```c
char buffer[4];
buffer[4] = 'a'
```
Behavior of this program is undefined, anything can happen and it all depends on the compiler choices.

Think for example if the value 'a' is actually controlled by the attacker, maybe it's an input reads from the keyboard or contents of a file. A malicious entity may try to exploit this UB to run arbitrary code.

IF we are lucky, this cause a page access fault at the OS level, but this can easily lead to remote code execution.


This is a very simple example of the number one vulnerability: buffer overflows.

Other memory corruption issues: 
- dangling pointers
- null dereference
- pointer arithmetic bugs
- double frees
- no clean initialization after allocation
- no validity checks of pointers

Using unsafe library functions (printf, gets, strcpy,...)

This can cause both heap and stack overflow, leading to data corruption and RCE.

Defenses:
- Stack canaries
- ASLR
- Non executable memory
- Static and/or dynamic analysis

These does not solve control flow attacks, where the memory is overwritten to a valid address that can still be controlled:
Return to LibC, Return oriented Programming.

