(* ---------------------------------------------------------------------- *)
(* shopping cart *)

(* We'll talk about the code for the controller next week! 
   It's okay if you don't understand it yet.  
*)

fun controller (view : 'model -> string,
                respond : 'model * string -> 'model,
                m : 'model) : unit =
    let val () = print (view m ^ "\n")
    in
        case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "text input errored"
          | SOME input => controller(view, respond, respond(m,String.substring(input,0,String.size input -1)))
    end

fun intToString(x : int) : string = Int.toString x

(* TASK 1: Add constructors to the type 'model' to model the states of the shopping application *)
    
datatype model = 
    EnterName
    | shopping of (string * int * string) list * (string * int * string) list * string
    | checkout of (string * int * string) list * (string * int * string) list * string

(*Purpose: Sums the total value of all items in the cart*)
fun pay (cart : (string * int * string) list) : int =
    case cart of 
        [] => 0
        | x :: xs => let val (x: string, y: int, z: string) = x
                        in y + pay(xs) end

(*Purpose: contains a list of products a customer can select from*)
fun products(void):(string * int * string) list =
    [("apples", 1, "pound"), ("bananas", 2, "bunch"), ("cookies", 2, "box")]

(*Purpose: confirms if a product is available and adds it to the cart.
If the product is not available, the original cart is returned.*)
fun products_list(cart: (string * int * string) list, items: (string * int * string) list, item: string) : (string * int * string) list =
    case items of 
        [] => cart
        | (x: string, y: int, z: string) :: xs => case item = x of
                                                    true => cart @ [(x,y,z)]
                                                    | false => products_list(cart, xs, item)

(*Purpose: displays thhe products in the cart*)
fun print_products(cart: (string * int * string) list) : string =
    case cart of
        [] => ""
        | x :: xs => let val (x: string, y: int, z: string) = x
                        in x ^ "," ^ print_products(xs) end
                                                   
    
(* utility functions for writing tests *)    
fun tests (s : string) (n : string) (m : string) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ m ^ "\n    Got: " ^  n ^ "\n")

fun test_model (s : string) (n : model) (m : model) : unit =
    case n=m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED\n")

            
(* TASK 2: write a function that displays the model as a string *)
    
fun view (m : model) : string = 
    case m of
        EnterName => "Please enter your name: "
        | shopping(x, y, z) => "Hi, " ^ z ^ ". What would you like to buy? " ^
                                                "\n apples $1/pound"^
                                                "\n bananas $2/bunch"^
                                                "\n cookies $2/box"^
                                                "\n Or say 'checkout' to check out"
        | checkout(x, y, z) => "Hi, " ^ z ^ "\n Your cart contains : " ^ print_products(x) ^
                                                "\n I will charge you $" ^ intToString(pay x) ^
                                                ". \n Type 'pay' to pay."

fun test_view() =
    ()

    
(* TASK 3: write a function that updates the model based on user input *)
    
fun respond(m : model, text : string) : model = 
    case m of  
        EnterName => shopping ([], [], text)
        | shopping (x, y, z) => (case text of
                                    "checkout" => checkout(x, y, z)
                                    | _ => shopping(products_list(x, products y, text), y, z))
        | checkout(x, y, z) => (case text of
                                    "pay" => EnterName
                                    | _ => checkout(x, y, z))                            

fun test_respond() =
    ()
    
(* TASK 4: choose an initial state, fill it, and uncomment this, 
           then run() will run the application*) 
    
fun run() = controller(view,respond, EnterName)


