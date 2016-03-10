module assignment3

(*
Exercise 3.1
Consider the definition of type ’a BinTree on slide 30. Write a function
inOrder : ’a BinTree -> ’a list
that makes an in-order traversal of the tree and collect the elements in a result list. In-order traversal is defined on
slide 32.
*)

//Definition of a binary tree
type 'a BinTree = 
    |Leaf
    |Node of 'a * 'a BinTree * 'a BinTree;;

//Inorder travesal of tree and collecting items in result list
let rec inOrder tree = 
    match tree with
    |Leaf -> []
    |Node(x,tl,tr) -> (inOrder tl) @ [x] @ (inOrder tr);; 

//Example 
let floatBinTree =
    Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)));;

inOrder floatBinTree;;


(*
    Exercise 3.2 Write a function
    mapInOrder : (’a -> ’b) -> ’a BinTree -> ’b BinTree
    that makes an in-order traversal of the binary tree and apply the function on all nodes in the tree.
    Can you give an example of why mapInOrder might give a result different from mapPostOrder, but the
    result tree returned in both cases is still the same.

*)

let rec mapInOrder f tree =
    match tree with
    | Leaf -> Leaf
    | Node(x,tl,tr) -> Node(f x, mapInOrder f tl, mapInOrder f tr);;
    
mapInOrder (fun x -> x+2.0) floatBinTree;;  //Applies the function f(x) = x+2 on all nodes in a given tree

//Reason why mapInOrder might give a result different from mapPostOrder but returns same result tree is because of the
// differences in traversal of the tree. However, they will both return the same tree because both function will iterate over all nodes in the tree. 
// The main differences are when the root is visited.


(*
Exercise 3.3 Write a function

    foldInOrder : (’a -> ’b -> ’b) -> ’b -> ’a BinTree -> ’b
that makes an in-order traversal of the tree and folds over the elements.

For instance, given the tree
    let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
    Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

the application

foldInOrder (fun n a -> a + n) 0.0 floatBinTree

*)


//Fold over elements in inOrder traversal of tree
let foldInOrder f e tree = 
    let list = inOrder tree
    List.fold f e list;;

 //Example (Uses float bin tree from prev task)
foldInOrder (fun n a -> a + n) 0.0 floatBinTree //returns 764.0. which is the correct answer


(*
Exercise 3.4 Complete the program skeleton for the interpreter presented on slide 28 in the slide deck from the
lecture 5 about finite trees.
Define 5 examples and evaluate them.
The declaration for the abstract syntax for arithmetic expressions follows the grammar (slide 23)
*)  

//Defines a state
type state = Map<string,int>;;

//lookup function (Used to find a value for a given element in a state)
let lookup (x:string) s= Map.find x s;;

//update function (Updates a given value v, for a given key x in a state s )
let update x v s = Map.add x v s;;


type aExp = (* Arithmetical expressions *)
| N of int (* numbers *)
| V of string (* variables *)
| Add of aExp * aExp (* addition *)
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp(* subtraction *)
| Inc of string ;; (*Exercise 3.8 - look up*)

//A is the function used to evaluate arithmetic expressions with arguments a being the expression and s being the state of the program.
let rec A a s =
    match a with
    |N n -> (n,s) // n is an instance of expression N 
    |V x -> (Map.find x s,s) 
    |Add(a1, a2) -> (fst(A a1 s) + fst(A a2 s),s) //fst is a build in-command to retrieve first element in a tuple
    |Mul(a1, a2) -> (fst(A a1 s) * fst(A a2 s),s)
    |Sub(a1, a2) -> (fst(A a1 s) - fst(A a2 s),s)
    |Inc x ->
        let v = lookup x s //retrieve x from map
        let newV = v+1 // increment value of x
        let updatedState = update x newV s //update value in state
        (newV,updatedState) // return pair of (x+1,state)
    
// Arithmetic expression examples 
let s = Map.add "x" 3 Map.empty;;
let exp1 = A (Add(N(1),N(1))) s;; //1+1 = 2
let exp2 = A (      Mul((Add(N(5),N(10)), (Sub(N(3),V("x")))))) s;; // (5+10)*(3-3) = 0

type bExp = (* Boolean expressions *)
| TT (* true *)
| FF (* false *)
| Eq of aExp * aExp (* equality *)
| Lt of aExp * aExp (* less than *)
| Neg of bExp (* negation *)
| Con of bExp * bExp;; (* conjunction *)

// B is the function used to evaluate boolean expressions with arguments b being expressions and s being the state of the program. 
let rec B a s =
    match a with
    | TT -> true    (* true *)
    | FF -> false   (* false *)
    | Eq(x,y) -> A x s = A y s (* equality *)
    | Lt(x,y) -> A x s < A y s (* less than *)
    | Neg x ->  not(B x s) (* negation *)
    | Con(x,y) -> B x s && B y s(* conjunction *);;

// Boolean expression examples 
let exp3 = B ( Neg(Eq(N(3),V("x"))) ) s // !(3=3) false
let exp4 = B ( Con(Eq(N(3),V("x")), Eq(N(4),N(4))) ) s // (3=3)&&(3=3) true

// Statements: Abstract Syntax
type stm = (* statements *)
| Ass of string * aExp (* assignment *)
| Skip
| Seq of stm * stm (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else *)
| While of bExp * stm (* while *)
| IT of bExp * stm (*if then*)
| RU of bExp * stm (*Repeat until*)

// Interpreter for Statements in Imperative Program
let rec I stm s =
    match stm with
    | Ass(x,a) -> 
        let (value, newS) = A a s
        update x value newS // updates state with value a for key x 
    | Skip -> s
    | Seq(stm1, stm2) -> I stm1 (I stm2 s) // Statement 1 awaits statement 2 
    | ITE(b,stm1,stm2) -> 
        if B b s 
        then I stm1 s 
        else I stm2 s
    | While(b, stm1) -> // Execute statement recursively like a while loop 
        if B b s 
        then I stm (I stm1 s) 
        else I Skip s// (I stm1 s runs the while loop again with the new state)
    | IT(b,stm1) -> 
        if B b s 
        then I stm1 s 
        else I Skip s
    | RU(b,stm1) -> I (Seq(stm, While(Neg b, stm))) s;;

// Statement example 
(*
    Example of I While(...) S ;

    While(x<10)
    {
        x++;
    }
*)
let exp5 = I (While(Lt(V("x"),N(10)), Ass("x",(Add(V("x"),N(1)))))) s


(* Exercise 3.5 Extend the abstract syntax and the interpreter with if-then and repeat-until statements.
Again we refer to slide 28 in the slide deck from the lecture 5.
    See Interpreter for Statements and Abstract Syntax in exercise 3.4 for extended version 
    We have added the following in the I function 
    IT = if then
    RU = repeat until



*)  



(*
Exercise 3.6 Suppose that an expression of the form inc(x) is added to the abstract syntax. It adds one to the
value of x in the current state, and the value of the expression is this new value of x.
How would you refine the interpreter to cope with this construct?
Again we refer to slide 28 in the slide deck from the lecture 5


    See definition for A in exercise 3.4 where we now include the state
    fst built-in function is used to take the value in the pairs of (expression,state) 
    Now expressions have side effects due to state argument   
*)


(*
Exercise 3.7
    Postfix form is a particular representation of arithmetic expressions where each operator is
    preceded by its operand(s), for example:
    (x + 7.0) has postfix form x 7.0 +
    (x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
    Declare an F# function with type Fexpr -> string computing the textual, postfix form of
    expression trees from Section 6.2.
*)

// Expression tree represented by values of recursively defined type
type Fexpr = 
    | Const of float 
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr


// Computes textual postfix form of expression tree represented by Fexpr 
let rec toString s =
    match s with
    | Const x -> string x
    | X -> "x"
    | Add(x,y) ->  (toString x)  + " " + (toString y) + " + "
    | Sub(x,y) ->  (toString x)  + " " + (toString y) + " - "
    | Mul(x,y) ->  (toString x)  + " " + (toString y) + " * "
    | Div(x,y) ->  (toString x)  + " " + (toString y) + " / "
    | Sin x -> (toString x) + " Sin " 
    | Cos x -> (toString x) + " Cos "
    | Log x -> (toString x) + " Log " 
    | Exp x ->(toString x) + " Exp "

// Example 
toString (Add(Const(1.0),(Div(Const(10.0),(Const(2.0))))))

(*
Exercise 3.8 HR exercise 6.8

*)

// Type for representing stack used to hold float numbers
type stack = float list;;

// Instruction set of calculator
type instruction =
    | ADD 
    | SUB 
    | MULT 
    | DIV 
    | SIN
    | COS 
    | LOG 
    | EXP 
    | PUSH of float

// Pop one element in stack
let popSingle = function
| [] -> failwith "Stack is empty"
| v::vs -> (v,vs);; //Pops first element and pair it with rest of stack

// Pop two elements from stack used in binary operation 
let popDouble stack = //Pops two elements from stack 
    let (v1, rest1) = popSingle stack // Pop first element
    let (v2, rest2) = popSingle rest1 // Pop second element in stack
    (v1,v2,rest2) // Return first two elements and rest of stack

// Put result of operation on two numbers back into stack
let applyBinOpr opr s = 
    let (v1,v2,vRest) = popDouble s // v1, v2 and rest of stack
    (opr v1 v2)::vRest;; // Put result in stack

// Pop single number for operation, e.g. sinus 
let applyBinUnoOpr opr s = 
    let (v1, vRest) = popSingle s
    (opr v1)::vRest;;
  
// Push item x onto stack s     
let push x s = x::s;;

// 1) Function to interpret execution of single instruction s 
let intpInstr s = function
 |ADD -> applyBinOpr (+) s // Give + as operation argument
 |SUB -> applyBinOpr (-) s
 |MULT -> applyBinOpr (*) s
 |DIV -> applyBinOpr (/) s 
 |SIN -> applyBinUnoOpr (System.Math.Sin) s
 |COS -> applyBinUnoOpr (System.Math.Cos) s
 |EXP -> applyBinUnoOpr (System.Math.Exp) s
 |LOG -> applyBinUnoOpr (System.Math.Log) s
 |PUSH x -> push x s;; 
 
 // 2) Program used to calculate list of instructions with function that interpret execution of program
 let intpProg instrList = 
    let rec execute (stack: float list) instrList =
        match instrList with
        | [] -> stack.Head
        | i::rest ->    let result = intpInstr stack i
                        execute result rest
    execute [] instrList;;


(*
    Exercise 3.8.3
*)
    
// Function that takes Fexpr for expression tree and gives float value
// Represents a stack that collects operations with left side and right side
// Stack gradually creates stack with numbers and instructions 
// Example: Add(Const(1), Const(1)); puts [Push(1)][Push(1)][ADD] into stack 
// Recursion used to allow multiple nested instructions 
let rec trans expr f =
    match expr with
    |Const(v) -> [PUSH(v)]
    |Add(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [ADD] 
    |Sub(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [SUB]
    |Mul(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [MULT]
    |Div(leftSide, rightSide) -> (trans leftSide f) @ (trans rightSide f) @ [DIV]
    |Sin(constant) -> (trans constant f) @ [SIN]
    |Cos(constant) -> (trans constant f) @ [COS]
    |Log(constant) -> (trans constant f) @ [LOG]
    |Exp(constant) -> (trans constant f) @ [EXP]

(*
    Exercise 3.9    

    Make signature and implementation files for a library of complex numbers with overloaded
    arithmetic operators (cf. Exercise 3.3).
*)

//Complex numbers Signature (interface)
module Complex
type complex =
val ( +. ) : complex -> complex -> complex //sum, complex sum
val ( -. ) : complex -> complex -> complex //sub, complex difference
val ( /. ) : complex -> complex -> complex //div, omplex division
val ( *. ) : complex -> complex -> complex //mul, complex product

//Complex numbers implementation (similar to a class)
module Complex 
type complex = C of float*float
let ( +. ) C(x1,y1) C(x2,y2) = (x1+x2, y1+y2)
let ( -. ) (x1,y1) (x2,y2) = ( (x1,y1) +. (-x2,-y2) )
let ( /. ) (x1,y1) (x2,y2) = ( (x1/x1*x1 + y1*y1) , (-y1/x1*x1 + y1*y1) ) *. (x2,y2)
let ( *. ) (x1:float,y1:float) (x2:float,y2:float) = (x1*x2-y1*y2, y1*x2+x1*y2)


