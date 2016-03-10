

// Exercise 3.1

(* 

Consider the definition of type ’a BinTree on slide 30. 
Write a function inOrder : ’a BinTree -> ’a list
that makes an in-order traversal of the tree and collect the elements in a result list. 
In-order traversal is defined on slide 32.

*)

// Definition of a binary tree 
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree ;; 

//type BinTree<'a> = | Leaf
//                   | Node of BinTree<'a> * 'a * BinTree<'a> ;; 



// Function that makes in-order traversal of tree and collects items in result list 

let rec inOrder = function
    | Leaf -> []
    | Node(x, tl, tr) -> (inOrder tl) @ [x] @ (inOrder tr) ;; 

let rec inOrderTwo tree = 
    match tree with
    | Leaf -> []
    | Node(x,tl, tr) -> (inOrderTwo tl) @ [x] @ (inOrderTwo tr) ;; 

// Example 
let root = 43 
let leftSubtree = Node(25, Node(56, Leaf, Leaf), Leaf) 
let rightSubtree = Node(562, Leaf, Node(78, Leaf, Leaf))
let treeExample = Node(root, leftSubtree, rightSubtree) 
let treeExampleHard = Node(43, Node(25, Node(56, Leaf, Leaf), Leaf), Node(562, Leaf, Node(78, Leaf, Leaf))) ;; 

inOrder treeExample ;; 

// Exercise 3.2

(*

Write a function mapInOrder:(’a -> ’b)->’aBinTree->’bBinTree
that makes an in-order traversal of the binary tree and apply the function on all nodes in the tree.
Can you give an example of why mapInOrder might give a result different from mapPostOrder, 
but the result tree returned in both cases is still the same.

*)


let rec mapInOrder f tree = 
    match tree with 
    | Leaf -> Leaf
    | Node(x,tl,tr) -> Node(f x, mapInOrder f tl, mapInOrder f tr) ;; 

// Applies function f(x)=x+2 on all elements in a given tree 
mapInOrder (fun x -> x+2) treeExample 

// Example on why mapInOrder might give different result from mapPostOrder but result tree is same


// Exercise 3.3 

(*

Write a function
foldInOrder : (’a -> ’b -> ’b) -> ’b -> ’a BinTree -> ’b
that makes an in-order traversal of the tree and folds over the elements. 
For instance, given the tree 

let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
                             Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
the application

foldInOrder (fun n a -> a + n) 0.0 floatBinTree

returns 764.0.

*)

// Tree example 
let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
                             Node(562.0, Leaf, Node(78.0, Leaf,Leaf))) ;; 

// Fold over elements in in-order traversal 
let foldInOrder f e tree = 
    let list = inOrder tree 
    List.fold f e list ;; // Apply accumulated function f to node elements from start to end

// Correct result returned (764)
foldInOrder (fun n a -> a + n) 0.0 floatBinTree ;; 

// Exercise 3.4

(*
Exercise 3.4 Complete the program skeleton for the interpreter presented on slide 28 in the slide deck from the
lecture 5 about finite trees.
Define 5 examples and evaluate them.
The declaration for the abstract syntax for arithmetic expressions follows the grammar (slide 23):
type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *)
The declaration of the abstract syntax for boolean expressions is defined as follows (slide 25

type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp (* conjunction *)
The conjunction of two boolean values returns true if both values are true.
The abstract syntax for the statements are defined as below (slide 26):
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)

*)

// State with string keys and int values 
type state = Map<string, int> ;; 

// State example
let s = Map.add "x" 1 Map.empty ;; 

// Lookup function used to find value for given element in state in exercise 3.6
let lookup x s = Map.find x s ;;

// Updates a value v for a given key x in a state s
let update x v s = Map.add x v s ;;

// Arithmetic Expressions (data) 
type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtraction *) 
| Inc of string ;; (* Exercise 3.6 *)

// A is the function used to evaluate arithmetic expressions (functions) 
// with arguments a being the expression and s being the state of the program 
let rec A a s = 
    match a with
    | N n -> (n,s) // n is an instance of expression N 
    | V x -> (Map.find x s, s)  
    | Add (x,y) -> (fst(A x s) + fst(A y s), s)  
    | Mul (x,y) -> (fst(A x s) * fst(A y s), s) 
    | Sub (x,y) -> (fst(A x s) - fst(A y s), s) 
    // Increment and return x 
    | Inc x -> let lookupValue = lookup x s // Find value
               let newValue = lookupValue+1 // Increment value
               let updatedState = update x newValue s // Update state
               (newValue, updatedState) ;; // Return pair of (x+1, state)

// Example of arithmetic expression
let exp1 = A (Add(N(1),N(1))) s ;; // 1 + 1 

// Boolean expressions
type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *) // Two numbers from aExp 
| Lt of aExp * aExp (* less than *) // We compare values (e.g. numbers) from aExp
| Neg of bExp (* negation *) // Only requires one boolean expression 
| Con of bExp * bExp ;; (* conjunction *) // And operator requires two boolean expressions

// B is the function used to evaluate boolean expressions 
// with arguments b beng expressions and s being state 

let rec B b s = 
    match b with
    | TT -> true
    | FF -> false 
    | Eq(x,y) -> A x s = A y s // Use function A to retrieve value from aExp 
    | Lt(x,y) -> A x s < A y s 
    | Neg x -> not(B x s) // Negate bExp 
    | Con(x,y) -> B x s && B y s ;; 

// Boolean example
let exp2 = B (Eq(N(1), N(1))) s ;;

// Statements: Abstract Syntax
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)
| IT of bExp * stm // If then in exercise 3.5 
| Repeat of bExp * stm ;;// Repeat until: opposite of while, runs until true in exercise 3.5 

// Interpreter for Statements in Imperative program 
let rec I stm s =
    match stm with
    | Ass(x,a) -> // update x (A a s)  s // Updates state with value a for key x  
                  let (value, newState) = A a s
                  update x value newState
    | Skip -> s // Returns the same state 
    | Seq(stm1, stm2) -> I stm1 (I stm2 s) // Statement 1 awaits for statement 2 
    | ITE(b,stm1,stm2) -> if B b s then (I stm1 s) else (I stm2 s) 
    | While(b, stm1) -> if B b s then I stm (I stm1 s) // Execute statement recursively like a while loop 
                                 else I Skip s 
    | IT(b, stm1) -> if B b s then (I stm1 s) else s // If then in exercise 3.5 
    | Repeat(b, stm1) -> I (Seq(stm1, While (Neg b, stm1))) s ;; // Repeat until in exercise 3.5 

// Statement example adds 1 to value associated with x 5 times 
let bool = Lt(V("x"),N(5));;
 
let exp3 = I (While( bool, Ass("x", Add(V("x"), N(1))))) s ;;
Map.find "x" exp3;;

// Exercise 3.5

// See above Interpreter for Statements and Abstract syntax in exercise 3.4 for extended versions 


// Exercise 3.6

(* 

Suppose that an expression of the form inc(x) is added to the abstract syntax. It adds one to the
value of x in the current state, and the value of the expression is this new value of x.
How would you refine the interpreter to cope with this construct?
Again we refer to slide 28 in the slide deck from the lecture 5

*)

// See definition for A in exercise 3.4 where we now include the state
// fst built-in function is used to take the value in the pairs of (expression,state) 
// Now expressions have side effects due to state argument 


// Exercise 3.7 HR exercise 6.2

(* 

Postfix form is a particular representation of arithmetic expressions
 where each operator is preceded by its operand(s), for example:
(x + 7.0) has postfix form x 7.0 +
(x+7.0)*(x−5.0) has post fix form x 7.0 + x 5.0 − *

Declare an F# function with type Fexpr -> string computing the textual, 
postfix form of expression trees from Section 6.2.

*)

// Expression tree represented by values of recursively defined type 
type Fexpr = 
| Const of float
| X 
| Add of Fexpr * Fexpr
| Sub of Fexpr * Fexpr
| Mul of Fexpr * Fexpr
| Div of Fexpr * Fexpr 
| Sin of Fexpr
| Cos of Fexpr
| Log of Fexpr
| Exp of Fexpr ;; 


// Computes textual postfix form of expression tree represented by Fexpr 
let rec textualPostForm s = 
    match s with
    | Const x -> string x 
    | X -> "x"
    | Add(x,y) -> (textualPostForm x) + " " + (textualPostForm y) + " +"
    | Sub(x,y) -> (textualPostForm x) + " " + (textualPostForm y) + " -"
    | Mul(x,y) -> (textualPostForm x) + " " + (textualPostForm y) + " *"
    | Div(x,y) -> (textualPostForm x) + " " + (textualPostForm y) + " /"
    | Sin x -> (textualPostForm x) + " " + " sin"
    | Cos x -> (textualPostForm x) + " " + " cos"
    | Log x -> (textualPostForm x) + " " + " log"
    | Exp x -> (textualPostForm x) + " " + " exp" ;; 

// Example
textualPostForm (Add(Const(1.0), Const(2.0))) ;; 

// Exercise 3.8 HR exercise 6.8

(* 

Simple calculator for addition, subtraction, multiplication and division of floats 

*)

// Instruction set of calculator 
type Instruction = 
| ADD 
| SUB 
| MULT 
| DIV 
| SIN
| COS
| LOG
| EXP 
| PUSH of float ;; 

// 1) Type for representing Stack used to hold floatn umbers 
type Stack = float list ;; 

// Stack helper functions

// Pop one element in stack 
let popSingle = function
| [] -> failwith "The stack is empty"
| v::vs -> (v,vs) ;; // Return first element and rest of stack

// Pop two elements from stack used in binary operation 
let popDouble s = 
    let (v1,rest1) = popSingle s // Pop first element
    let (v2, rest2) = popSingle rest1 // Pop second element in stack 
    (v1, v2, rest2) ;; // Return first two elements and rest of stack 

// Put result of operation on two numbers back into stack 
let applyBinOpr opr s = 
    let (v1, v2, rest) = popDouble s // v1, v2 and rest of stack
    (opr v1 v2) :: rest ;; // Put result in stack 

// Pop single number for operation, e.g. sinus
let applyOneOpr opr s = 
    let (v1, rest) = popSingle s
    (opr v1) :: rest ;; 

// Push item x onto stack s 
let push x s = x :: s ;; 

// 1) Function to interpret execution of single instruction s 
let intSInstruction s = function
    | ADD -> applyBinOpr (+) s // Give + as operation argument
    | SUB -> applyBinOpr (-) s 
    | MULT -> applyBinOpr (*) s
    | DIV -> applyBinOpr (/) s 
    | SIN -> applyOneOpr (System.Math.Sin) s 
    | COS -> applyOneOpr (System.Math.Cos) s  
    | LOG -> applyOneOpr (System.Math.Log) s
    | EXP -> applyOneOpr (System.Math.Exp) s
    | PUSH x -> push x s ;; 

// 2) Program used to calculate list of instructions with function that interpret execution of program

// intProg Instruction list -> float  
let intProg instructions =
    let rec executeProgram (stack: float list) instructions =
        match instructions with
        | []                -> stack.Head
        | instruction::rest -> let resultStack = intSInstruction stack instruction
                               executeProgram resultStack rest
    executeProgram [] instructions

// 3) Function trans: Fexpr * float -> Instruction list 

// Takes Fexpr for expression tree and gives float value 
// Constructs an expression tree that show how operations relate to the node elements 
let rec trans expr f =
    match expr with
    |Const(v) -> [PUSH(v)]
    | X -> [] // Not sure what to do with X as an unknown value  
    |Add(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [ADD] 
    |Sub(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [SUB]
    |Mul(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [MULT]
    |Div(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [DIV]
    |Sin(constant) -> (trans constant f) @ [SIN]
    |Cos(constant) -> (trans constant f) @ [COS]
    |Log(constant) -> (trans constant f) @ [LOG]
    |Exp(constant) -> (trans constant f) @ [EXP] ;; 

// Represents a stack that collects operations with left side and right side
// Stack gradually creates stack with numbers and instructions 
// Example: Add(Const(1), Const(1)); puts [Push(1)][Push(1)][ADD] into stack 
// Recursion used to allow multiple nested instructions 

// Exercise 3.9 HR exercise 7.2 

(* 

7.2 Make signature and implementation files for a library of complex numbers with overloaded
arithmetic operators (cf. Exercise 3.3).

*)

// Complex numbers Signature (interface)
module Complex 

type complex =  
val ( +. ) : complex -> complex -> complex // Complex sum 
val ( -. ) : complex -> complex -> complex // Complex difference
val ( *. ) : complex -> complex -> complex // Complex product
val ( /. ) : complex -> complex -> complex ;; // Complex division 

// Complex numbers implementation (class) 

module Complex 

type complex = C of float * float 
let ( +. ) (C(x1,y1)) (C(x2,y2)) = (x1+x2, y1+y2) 
let ( -. ) (x1,y1) (x2,y2) = ((x1,y1) +. (-x2,-y2)) 
let ( *. ) (x1,y1) (x2,y2) = (x1*x2)-(y1*y2) , (y1*x1)+(x1*y2) 
let ( /. ) (x1,y1) (x2,y2) = ( (x1/x1*x1 + y1*y1) , (-y1/x1*x1 + y1*y1) ) *. (x2,y2) 



