datatype value = Nil | ERROR of string | INT of int | STR of string | BOOL of string | NAME of string | UNIT_INT of string * int | UNIT_STR of string * string | UNIT_BOOL of string * string | FUNCT of string * string * string list * value list | INOUT of string * string * string list

datatype command = Push | Pop | True | False | Error | Add | Sub | Mul | Div | Rem | Neg | Swap | Quit | And | Or | Not | Equal | LessThan | If | Bind | Let | End | Fun | Return | FunEnd | Call | InOutFun

fun get(input : string) = 
    let
        val file = TextIO.openIn input 
        fun get file =
            case TextIO.inputLine file of
                SOME line => line :: get file
                | NONE => []
    in 
        get file before TextIO.closeIn file
end

fun write(input : string, outFile : string) =
    let
        val out = TextIO.openOut outFile
        val result = TextIO.output(out, input)
        val result = TextIO.closeOut out
    in
        input
    end

fun lookup(a, []) = ERROR(":error:")
    | lookup(a, c::d) =
        case c of
        UNIT_INT(x, y) => if a = x then INT(y) else lookup(a, d)
        | UNIT_STR(x, y) => if a = x then STR(y) else lookup(a, d)
        | UNIT_BOOL(x, y) => if a = x then BOOL(y) else lookup(a, d)
        | FUNCT(w,x,y,z) => if a = x then c else lookup(a, d)
        | INOUT(x,y,z) => if a = x then c else lookup(a,d)
        | _ => lookup(a, d)

fun search(a, []) = ERROR(":error:")
    | search(a, c::d) =
        case c of 
        UNIT_INT(x, y) => if a = x then c else lookup(a, d)
        | UNIT_STR(x, y) => if a = x then c else lookup(a, d)
        | UNIT_BOOL(x, y) => if a = x then c else lookup(a, d)
        | _ => lookup(a, d)


fun evalValue(v : value, name, []) = false
    | evalValue(v : value, name, c::d) = 
        let 
            val temp = lookup(name, c::d)
        in
            case temp of
            v => true
        end


fun flag(_, _, _, []) = []
    | flag(com, x, y, c::d) = 
        if x = 0 then []
        else if com = "div" then INT(y div x)::d 
        else INT(y mod x)::d

fun intToString i =
    if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

fun arith(_, _, _, []) = []
    | arith(com, x, y, c::d) =
        if com = "add" then (print (intToString x ^ " ADD " ^ intToString y ^ " | ") ; INT(x+y)::d)
        else if com = "sub" then  (print (intToString x ^ " SUB " ^ intToString y ^ " | ") ; INT(y-x)::d)
        else if com = "mul" then (print (intToString x ^ " MUL " ^ intToString y ^ " | ") ; INT(x*y)::d)
        else flag(com, x, y, c::d)

fun neg(x) = 
    case x of
    INT x => INT(~x)
    | _ => ERROR(":error:")

fun helper(_, _, _, [], []) = ([], [])
    | helper(com, x, y, c::d, []) = 
        (case x of 
        INT x => 
        if com = "neg" then (neg(INT x)::d, []) else
            (case y of 
            INT y => (arith(com, x, y, List.drop(d, 0)), [])
            | _ => (ERROR(":error:")::c::d, [])
            )  
        | _ => (ERROR(":error:")::c::d, []))
    | helper(com, x, y, c::d, e::f) = 
        (case x of 
        INT x => 
        if com = "neg" then (neg(INT x)::d, e::f) else
        (case y of 
            INT y => (arith(com, x, y, List.drop(d, 0)), e::f)
            | NAME y => 
                let
                    val h = lookup(y, e::f)
                in
                    case h of 
                    INT h => 
                        let
                            val quot = arith(com, x, h, List.drop(d, 0))
                        in
                            if length quot < 1 then (ERROR(":error:")::c::d, e::f) else (quot, e::f)
                        end
                    | _ => (ERROR(":error:")::c::d, e::f)
                end
            | _ => (ERROR(":error:")::c::d, e::f)
        )  
        | NAME x => 
            let
                val i = lookup(x, e::f)
            in
                (case i of 
                INT i => 
                if com = "neg" then (neg(INT i)::d, e::f) else 
                    (case y of
                        INT y => (arith(com, i, y, List.drop(d, 0)), e::f)
                        | NAME y => 
                            let 
                                val j = lookup(y, e::f)
                            in
                                case j of
                                INT j => (arith(com, i, j, List.drop(d, 0)), e::f)
                                | _ => (ERROR(":error:")::c::d, e::f)
                            end
                        | _ => (ERROR(":error:")::c::d, e::f)
                    )
                | _ => (ERROR(":error:")::c::d, e::f))
            end
        | _ => (STR(":error")::c::d, e::f)
        )
    | helper(_) = ([], [])

fun stackSizeFlag([]) = false
    | stackSizeFlag(c::d) = length d < 1

fun math(_, [], []) = (ERROR(":error:")::[], [])
    | math(com, c::d, []) = 
        if length d < 1 andalso com <> "neg" then (ERROR(":error:")::c::d, [])
        else if com = "neg" then helper(com, c, STR(""), c::d, [])
        else helper(com, c, List.nth(d, 0), c::d, [])
    | math(com, c::d, e::f) = 
        if length d < 1 andalso com <> "neg" then (ERROR(":error:")::c::d, e::f)
        else if com = "neg" then helper(com, c, STR(""), c::d, e::f)
        else helper(com, c, List.nth(d, 0), c::d, e::f)
    | math(_) = ([], [])

fun push(h, []) = [h]
    | push(h, x::xs) = h::x::xs

fun swap([]) = []
    | swap(c::d) =
        if length d < 1 then ERROR(":error:")::c::d else
        let 
            val a = c
            val b = List.nth(d, 0)
            val tail = List.drop(d, 1)
        in
            b::a::tail
        end

fun getStr(x) = 
    case x of 
    STR x => x
    | _ => ":error:"

fun isBool(s) = if s = ":true:" orelse s = ":false:" then true else false

fun evalBool(s) = if s = ":true:" then true else false

fun logic(com, a, b) =
    let 
        val x = evalBool(a)
        val y = evalBool(b)
    in
        if com = "and" then x andalso y else x orelse y
    end

fun boolHelper(com, a, b, []) = []
    | boolHelper(com, a, b, c::d) =
        if logic(com, a, b) then BOOL(":true:")::d
        else BOOL(":false:")::d

fun bool(com, [], []) = ([], []) 
    | bool(com, c::d, []) = 
        if length d < 1 then (ERROR(":error:")::c::d, [])
        else
            let 
                val a = c;
                val b = List.nth(d, 0)
            in
                case a of
                BOOL a => 
                (case b of 
                    BOOL b => (boolHelper(com, a, b, d), [])
                    | _ => (ERROR(":error:")::c::d, []))
                | _ => (ERROR(":error:")::c::d, [])
            end 
    | bool(com, c::d, e::f) =
        (if length d < 1 then (ERROR(":error:")::c::d, e::f)
        else
            let 
                val a = c;
                val b = List.nth(d, 0)
            in
                case a of
                BOOL a => 
                    (case b of 
                    BOOL b => (boolHelper(com, a, b, d), e::f)
                    | NAME b => 
                        let
                            val temp = lookup(b, e::f)
                        in
                            case temp of
                            BOOL t => (boolHelper(com, a, t, d), e::f)
                            | _ => (ERROR(":error:")::c::d, e::f)
                        end
                    | _ => (ERROR(":error:")::c::d, e::f))
                | NAME a => 
                    let 
                        val temp = lookup(a, e::f)
                    in
                        (case temp of 
                        BOOL t =>
                            (case b of
                            BOOL b => (boolHelper(com, t, b, d), e::f)
                            | NAME b => 
                                let
                                    val temp = lookup(b, e::f)
                                in
                                    case temp of 
                                    BOOL u => (boolHelper(com, t, u, d), e::f)
                                    | _ => (ERROR(":error:")::c::d, e::f)
                                end
                            | _ => (ERROR(":error:")::c::d, e::f)
                            )
                        | _ => (ERROR(":error:")::c::d, e::f)
                        )
                    end
                | _ => (ERROR(":error:")::c::d, e::f)
            end)
    | bool(_) = ([], [])

fun notHelper(bool) = if bool then BOOL(":false:") else BOOL(":true:")

fun not([], []) = (ERROR(":error:")::[], [])
    | not(c::d, []) =
        (case c of 
        BOOL c => (notHelper(evalBool(c))::d, [])
        | _ => (ERROR(":error:")::c::d, []))
    | not(c::d, e::f) = 
        (case c of
        BOOL c => (notHelper(evalBool(c))::d, e::f)
        | NAME n => 
            let
                val l = lookup(n, e::f)
            in
                case l of 
                BOOL l =>(notHelper(evalBool(l))::d, e::f)
                | _ => (ERROR(":error:")::c::d, e::f)
            end
        | _ => (ERROR(":error:")::c::d, e::f))
    | not(_) = ([], [])

fun compareHelper(com, x, y) =
    let
        fun equal(x, y) = if x = y then BOOL(":true:") else BOOL(":false:")
        fun lessThan(x, y) = if y < x then BOOL(":true:") else BOOL(":false:")
    in
        if com = "equal" then equal(x, y)
        else lessThan(x, y)
    end

fun compare(com, [], []) = (ERROR(":error:")::[], [])
    | compare(com, c::d, []) = 
        (if length d < 1 then (ERROR(":error:")::c::d, [])
        else 
            let
                val x = c
                val y = List.nth(d, 0)
                val list = List.drop(d, 1)
            in
                (case x of
                INT x =>
                    (case y of
                    INT y => (compareHelper(com, x, y)::list, [])
                    | _ => (ERROR(":error:")::c::d, []))
                | _ => (ERROR(":error:")::c::d, []))
            end)
    | compare(com, c::d, e::f) =
        (if length d < 1 then (ERROR(":error:")::c::d, e::f)
        else 
            let
                val x = c
                val y = List.nth(d, 0)
                val list = List.drop(d, 1)
            in
                (case x of
                INT x =>
                    (case y of
                    INT y => (compareHelper(com, x, y)::list, e::f)
                    | NAME y => 
                        let
                            val t = lookup(y, e::f)
                        in
                            case t of
                            INT t => (compareHelper(com, x, t)::list, e::f)
                            | _ => (ERROR(":error:")::c::d, e::f)
                        end
                    | _ => (ERROR(":error:")::c::d, e::f))
                | NAME x =>
                    (let
                        val temp = lookup(x, e::f)
                    in
                        case temp of
                        INT i => 
                            (case y of
                            INT y => (compareHelper(com, i, y)::list, e::f)
                            | NAME n =>
                                (let
                                    val temp1 = lookup(n, e::f)
                                in
                                    (case temp1 of
                                    INT j => (compareHelper(com, i, j)::list, e::f)
                                    | _ => (ERROR(":error:")::c::d, e::f)
                                    )
                                end)
                            | _ => (ERROR(":error:")::c::d, e::f))
                        | _ => (ERROR(":error:")::c::d, e::f)
                    end)
                | _ => (ERROR(":error:")::c::d, e::f))
            end
            )
    | compare(_) = ([], [])


fun if_([], []) = ([], [])
    | if_(c::d, []) =
        if length d < 2 then (ERROR(":error:")::c::d, []) else
        let
            val x = c
            val y = List.nth(d, 0)
            val z = List.nth(d, 1)
            val list = List.drop(d, 2)
        in
            case z of
            BOOL z => if z = ":true:" then (x::list, []) else (y::list, [])
            | _ => (ERROR(":error:")::c::d, []) 
        end
    | if_(c::d, e::f) =
        if length d < 2 then (ERROR(":error:")::c::d, e::f) else
        let
            val x = c
            val y = List.nth(d, 0)
            val z = List.nth(d, 1)
            val t = c::d
            val list = List.drop(d, 2)
        in
            case z of
            BOOL z => if z = ":true:" then (x::list, e::f) else (y::list, e::f)
            | NAME z => 
                let 
                    val temp = lookup(z, e::f)
                in
                    (case temp of
                    BOOL b => if b = ":true:" then (x::list, e::f) else (y::list, e::f)
                    | _ => (ERROR(":error:")::t, e::f))
                end
            | _ => (ERROR(":error:")::t, e::f) 
        end
    | if_(_) = ([], [])



fun isNameHelper([]) = true
    | isNameHelper(c::d) =
        if Char.isAlphaNum c orelse Char.isSpace c orelse Char.isPunct c then isNameHelper(d) else false

fun isName(s) =
    let
        val list = explode s
        val head = List.nth(list, 0)
    in
        Char.isAlpha head andalso isNameHelper(list)
    end


fun bindHelper(a, b, [], [], []) = ([], [])
    | bindHelper(a, b, c::d, tail, []) =
        (case a of
            INT a => (UNIT_INT(b, a)::tail, UNIT_INT(b, a)::[])
            | STR a => (UNIT_STR(b, a)::tail, UNIT_STR(b, a)::[])
            | BOOL a => (UNIT_BOOL(b, a)::tail, UNIT_BOOL(b, a)::[])
            | _ => (ERROR(":error:")::c::d, [])
        )
    | bindHelper(a, b, c::d, tail, e::f) = 
        (case a of
            INT a => (UNIT_INT(b, a)::tail, UNIT_INT(b, a)::e::f)
            | STR a => (UNIT_STR(b, a)::tail, UNIT_STR(b, a)::e::f)
            | BOOL a => (UNIT_BOOL(b, a)::tail, UNIT_BOOL(b,a)::e::f)
            | UNIT_INT(u, i) => (UNIT_INT(b, i)::tail, UNIT_INT(b, i)::e::f)
            | UNIT_STR(u, i) => (UNIT_STR(b, i)::tail, UNIT_STR(b, i)::e::f)
            | UNIT_BOOL(u, i) => (UNIT_BOOL(b, i)::tail, UNIT_BOOL(b, i)::e::f)
            | NAME a => 
                let 
                    val temp = lookup(a, e::f)
                in
                    case temp of
                    INT i => (UNIT_INT(b, i)::tail, UNIT_INT(b, i)::e::f)
                    | STR s => (UNIT_STR(b, s)::tail, UNIT_STR(b, s)::e::f)
                    | BOOL l => (UNIT_BOOL(b, l)::tail, UNIT_BOOL(b, l)::e::f)
                    | _ => (ERROR(":error:")::c::d, e::f)
                end
            | _ => ([], []))
    | bindHelper(_) = ([], [])


fun bind([], []) = ([], [])
    | bind(c::d, []) = 
        if length d < 1 then (ERROR(":error:")::c::d, []) else
        let
            val a = c
            val b = List.nth(d, 0)
            val tail = List.drop(d, 1)
        in 
            case b of
            NAME b => bindHelper(a, b, c::d, tail, [])
            | _ => (ERROR(":error:")::c::d, [])
        end
    | bind(c::d, e::f) =
        if length d < 1 then (ERROR(":error:")::c::d, e::f) else
        let
            val a = c
            val b = List.nth(d, 0)
            val tail = List.drop(d, 1)
        in 
            case b of
            NAME n => 
                let
                    fun delete (item : value, list : value list) = List.filter(fn x => x <> item) list
                    val v = search(n, e::f)
                in
                    case v of
                    ERROR v => bindHelper(a, n, c::d, tail, e::f)
                    | _ => bindHelper(a, n, c::d, tail, e::f)
                end
            | _ => (ERROR(":error:")::c::d, e::f)
        end
    | bind(_) = ([], [])

fun command(com, value, [], []) =
    (case com of 
        Push => (value::[], [])
        | True => (BOOL(":true:")::[], [])
        | False => (BOOL(":false:")::[], [])
        | Let => ([], [])
        | _ => (ERROR(":error:")::[], [])
    )
    | command(com, value, c::d, []) = 
        (case com of
        Push => (value::c::d, [])
        | Pop => (d, [])
        | True => (BOOL(":true:")::c::d, [])
        | False => (BOOL(":false:")::c::d, [])
        | Add => math("add", c::d, [])
        | Sub => math("sub", c::d, [])
        | Mul => math("mul", c::d, [])
        | Div => math("div", c::d, [])
        | Rem => math("rem", c::d, [])
        | Neg => math("neg", c::d, [])
        | Swap => (swap(c::d), [])
        | And => bool("and", c::d, [])
        | Or => bool("or", c::d, [])
        | Not => not(c::d, [])
        | Equal => compare("equal", c::d, [])
        | LessThan => compare("lessThan", c::d, [])
        | If => if_(c::d, [])
        | Bind => bind(c::d, [])
        | Let => (c::d, [])
        | End => (c::d, [])
        | FunEnd =>(c::d, [])
        | _ => (ERROR(":error:")::c::d, [])
        )
    | command(com, value, [], e::f) = 
        (case com of
        Push => (value::[], e::f)
        | Pop => (ERROR(":error:")::[], e::f)
        | True => (BOOL(":true:")::[], e::f)
        | False => (BOOL(":false:")::[], e::f)
        | Let => ([], e::f)
        | End => ([], e::f)
        | _ => (ERROR(":error:")::[], e::f)
        )
    | command(com, value, c::d, e::f) = 
        (case com of
        Push => (value::c::d, e::f)
        | Pop => (d, e::f)
        | True => (BOOL(":true:")::c::d, e::f)
        | False => (BOOL(":false:")::c::d, e::f)
        | Add => math("add", c::d, e::f)
        | Sub => math("sub", c::d, e::f)
        | Mul => math("mul", c::d, e::f)
        | Div => math("div", c::d, e::f)
        | Rem => math("rem", c::d, e::f)
        | Neg => math("neg", c::d, e::f)
        | Swap => (swap(c::d), e::f)
        | And => bool("and", c::d, e::f)
        | Or => bool("or", c::d, e::f)
        | Not => not(c::d, e::f)
        | Equal => compare("equal", c::d, e::f)
        | LessThan => compare("lessThan", c::d, e::f)
        | If => if_(c::d, e::f)
        | Bind => bind(c::d, e::f)
        | Let => (c::d, e::f)
        | End => (c::d, e::f)
        | FunEnd => (c::d, e::f)
        | _ => (ERROR(":error:")::c::d, e::f)
        )

 
fun isNum(xs) = List.all (Char.isDigit) (explode xs)

fun isString(s) =
    let
        val list = explode s
    in
        List.nth(list, 0) = #"\"" andalso List.last(list) = #"\""
    end

fun removeChar(s : string, item : char) =
    let 
        val list = explode s
        fun delete (item : char , list : char list) = List.filter(fn x => x <> item) list
    in
        String.implode(delete(item, list))
    end

fun isNeg(s) =  
    let 
        val list = explode s
        val str = removeChar(s, #"-")
    in
        isNum(str) andalso List.nth(list, 0) = #"-"
    end

fun getCom(s : string) = 
    let
         val list = String.tokens (fn c => c = #" ") s
    in
        List.nth(list, 0)
    end

fun getVal(s : string) = 
    if size s > 4 then String.extract(s, 5, NONE)
    else String.extract(s, 2, NONE)

fun getFun(s) = String.extract(s, 4, NONE)

fun removeChar(s : string, item : char) =
    let 
        val list = explode s
        fun delete (item, list : char list) = List.filter(fn x => x <> item) list
    in
        String.implode(delete(item, list))
    end

fun intFromString s =
    case Int.fromString s of
         SOME i => i
           | NONE => raise Fail ("THE STRING :" ^ s)

fun infixToNeg(s) = 
    let
        val list = explode s
        val tail = List.drop(list, 0)
        val pos = String.implode(tail)
        val num = intFromString pos
        val fin = INT(num)
    in
        fin
    end

fun output([], outFile : string) = ""
    | output(a::b : value list, outFile : string) =
        case a of 
            INT a => 
                let
                    val t = intToString a
                in
                    write(("" ^ t ^ "\n") ^ output(b, outFile), outFile)
                end
            | STR a => write(("" ^ a ^ "\n") ^ output(b, outFile), outFile)
            | BOOL a => write(("" ^ a ^ "\n") ^ output(b, outFile), outFile)
            | NAME a => write(("" ^ a ^ "\n") ^ output(b, outFile), outFile)
            | ERROR a => write(("" ^ a ^ "\n") ^ output(b, outFile), outFile)
            | _ => write(("" ^ ":unit:" ^ "\n") ^ output(b, outFile), outFile)

fun evalCommand(c) =
    case c of
    "push" => Push
    | "pop" => Pop
    | ":true:" => True
    | ":false:" => False
    | "add" => Add
    | "sub" => Sub
    | "mul" => Mul
    | "div" => Div
    | "rem" => Rem
    | "neg" => Neg
    | "swap" => Swap
    | "and" => And
    | "or" => Or
    | "not" => Not
    | "equal" => Equal
    | "lessThan" => LessThan
    | "if" => If
    | "bind" => Bind
    | "let" => Let
    | "end" => End
    | "fun" => Fun
    | "return" => Return
    | "funEnd" => FunEnd
    | "call" => Call
    | "inOutFun" => InOutFun
    | "quit" => Quit
    | _ => Error

fun evalElem(e) =
    if isNum(e) then INT(intFromString(e))
    else if isNeg(e) then infixToNeg(e)
    else if isNum(removeChar(e, #".")) then ERROR(":error:")
    else if isString(e) then STR(removeChar(e, #"\""))
    else if isName(e) then NAME(e)
    else ERROR(":error:")

fun removeSpaceHelper cd =
  case cd of
    [] => ([], [])
  | c :: cd' =>
      if Char.isSpace c then ([], cd')
      else let val (l, r) = removeSpaceHelper cd'
           in (c :: l, r)
           end

fun removeSpace s =
  let
    val (l, r) = removeSpaceHelper (String.explode s)
  in
    (String.implode l, String.implode r)
  end


fun evalInOut(line) = 
    let
        val list = removeSpace(line)
        val expr = #2 list
        val tup = removeSpace(expr)
        val name = #1 tup
        val arg = #2 tup
    in
        (name, arg)
    end

fun evalExpr(line) =
    let
        val expr = getFun(line)
        val tup = removeSpace(expr)
        val name = #1 tup
        val arg = #2 tup
    in
        (name, arg)
    end

fun fun_(name, arg, [], [], [], inOut) = ([], ERROR(":error:"))
    | fun_(name, arg, a::b, [], [], inOut) =
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
        in
            case com of 
            FunEnd => (b, ERROR(":error:"))
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val n = #1 expr
                    val argument = #2 expr
                    val funRes = fun_(n, argument, b, [], [], false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => ([], ERROR(":error:"))
                    | _ => fun_(name, arg, lines, [], [funct], false)
                end
            | _ => fun_(name, arg, b, [a], [], inOut)
        end
    | fun_(name, arg, a::b, [], e::f, inOut) =
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
        in
            case com of 
            FunEnd => if inOut then (b, INOUT(name, arg, [a])) else (b, FUNCT(name, arg, [], e::f))
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val n = #1 expr
                    val argument = #2 expr
                    val funRes = fun_(n, argument, b, [], e::f, false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => ([], ERROR(":error:"))
                    | _ => fun_(name, arg, lines, [], e::f, false)
                end
            | _ => fun_(name, arg, b, [a], e::f, inOut)
        end
    | fun_(name, arg, a::b, c::d, [], inOut) =
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
        in
            case com of 
            FunEnd => if inOut then (b, INOUT(name, arg, c::d @ [a])) else (b, FUNCT(name, arg, c::d, []))
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val n = #1 expr
                    val argument = #2 expr
                    val funRes = fun_(n, argument, b, [], [], false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => ([], ERROR(":error:"))
                    | _ => fun_(name, arg, lines, [], [funct], false)
                end
            | _ => fun_(name, arg, b, c::d @ [a], [], inOut)
        end
    | fun_(name, arg, a::b, c::d, e::f, inOut) =
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
        in
            case com of 
            FunEnd => if inOut then (b, INOUT(name, arg, c::d @ [a])) else (b, FUNCT(name, arg, c::d, e::f))
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val n = #1 expr
                    val argument = #2 expr
                    val funRes = fun_(n, argument, b, [], e::f, false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => ([], ERROR(":error:"))
                    | _ => fun_(name, arg, lines, [], e::f, false)
                end
            | _ => fun_(name, arg, b, c::d @ [a], e::f, inOut)
        end
    | fun_(_) = ([], ERROR(":error:"))


fun call(arg, param, a::b, ([], [])) = (ERROR(":error:"), [])
    | call(arg, param, a::b, ([], e::f)) = 
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                            Push => 
                            let 
                                val e = getVal(line)
                            in
                                case param of
                                ERROR err => param
                                | _ => if e = arg then param else evalElem(e)
                            end
                            | _ => STR("F")
                        )
        in  
            case elem of 
            ERROR err => (param, e::f)
            | _ => (
                case com of 
                Quit => (ERROR(":error:"), e::f)
                | Push => call(arg, param, b, command(com, elem, [], e::f))
                | Return => (ERROR(":error:"), e::f)
                | Fun =>
                    let 
                        val expr = evalExpr(line)
                        val name = #1 expr
                        val arg = #2 expr
                        val funRes = fun_(name, arg, b, [], [], false)
                        val lines = #1 funRes
                        val funct = #2 funRes
                    in
                        case funct of
                        ERROR err => call(arg, param, lines, (ERROR(":error:")::[], e::f))
                        | _ => call(arg, param, lines, (funct::[], [funct])) 
                    end
                | _ => call(arg, param, b, command(com, Nil, [], e::f))
            )
        end
    | call(arg, param, a::b, (c::d, e::f)) = 
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                            Push => 
                            let 
                                val e = getVal(line)
                            in
                                case param of
                                ERROR err => param
                                | _ => if e = arg then param else evalElem(e)
                            end
                            | _ => STR("F")
                        )
        in 
            case elem of 
            ERROR err => (param, e::f)
            | _ => 
            (
                case com of 
                Quit => (c, e::f)
                | Push => call(arg, param, b, command(com, elem, c::d, e::f))
                | Return => (c, e::f)
                | FunEnd => (Nil, e::f)
                | Fun =>
                    let 
                        val expr = evalExpr(line)
                        val name = #1 expr
                        val arg = #2 expr
                        val funRes = fun_(name, arg, b, [], e::f, false)
                        val lines = #1 funRes
                        val funct = #2 funRes
                    in
                        case funct of
                        ERROR err => call(arg, param, lines, (ERROR(":error:")::c::d, e::f))
                        | _ => call(arg, param, lines, (funct::[], funct::e::f)) 
                    end
                | Call => 
                    (case c of
                    NAME n => 
                        (let 
                            val funct = lookup(n, e::f)
                        in
                            case funct of
                            FUNCT(name, argument, lines, binds) => 
                                if length d < 1 then call(arg, param, b, (ERROR(":error:")::c::d, e::f)) else
                                let
                                    val parameter = List.nth(d, 0)
                                    val funRes = call(argument, parameter, lines, ([], e::f))
                                    val returnVal = #1 funRes
                                    val binds = #2 funRes
                                in
                                    case returnVal of
                                    ERROR err => call(arg, param, b, (returnVal::c::d, e::f))
                                    | _ => call(arg, param, b, (returnVal::List.drop(d, 1), binds))
                                end
                            | INOUT(name, argument, lines) =>
                                if length d < 1 then call(arg, param, b, (ERROR(":error:")::c::d, e::f)) else
                                let
                                    val parameter = List.nth(d, 0)
                                    val funRes = call(argument, parameter, lines, ([], e::f))
                                    val returnVal = #1 funRes
                                    val binds = #2 funRes
                                in
                                    case returnVal of
                                    ERROR err => call(arg, param, b, (returnVal::c::d, e::f))
                                    | NAME nam =>
                                        let
                                            val lu = lookup(nam, e::f)
                                        in
                                            case lu of
                                            ERROR err => call(arg, param, b, (returnVal::List.drop(d, 1), binds))
                                            | _ => call(arg, param, b, (lu::List.drop(d, 1), binds))
                                        end
                                    | _ => call(arg, param, b, (returnVal::List.drop(d, 1), binds))
                                end
                            | _ => call(arg, param, b, (ERROR(":error:")::c::d, e::f))
                        end)
                    | _ => call(arg, param, b, (ERROR(":error:")::c::d, e::f))
                    )
                | _ => call(arg, param, b, command(com, Nil, c::d, e::f))
                )
        end
    | call(_) = (ERROR(":error:"), [])

fun let_(g::h, ([], []), tof) =
    let
        val line = removeChar(g, #"\n")
        val com = evalCommand(getCom(line))
        val elem = (case com of
                    Push => evalElem(getVal(line))
                    | _ => ERROR(":error:")
                    )
        in
            case com of
            Quit => (h, [])
            | Push => let_(h, command(com, elem, [], []), tof)
            | Let => let_(h, ([], []), true)
            | Fun => 
                let 
                    val expr = evalExpr(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, h, [], [], false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => let_(lines, (ERROR(":error:")::[], []), tof)
                    | _ => let_(lines, ([funct], [funct]), tof) 
                end
            | _ => let_(h, command(com, Nil, [], []), tof)
        end
    | let_(g::h, ([], k::l), tof) =
        let
            val line = removeChar(g, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push => evalElem(getVal(line))
                        | _ => ERROR(":error:")
                        )
        in
            case com of
            Quit => (h, [])
            | Push => let_(h, command(com, elem, [], k::l), tof)
            | Let => 
                let
                    val stack = let_(h, ([], []), true)
                    val lines = #1 stack
                    val env = #2 stack
                    val top = List.nth(env, 0)
                in
                    if tof then let_(lines, (env, k::l), true) else let_(lines, ([top], []), true)
                end
            | End => (h, [])
            | _ => let_(h, command(com, Nil, [], k::l), tof)
        end
    | let_(g::h, (i::j, []), tof) =
        let
            val line = removeChar(g, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push => evalElem(getVal(line))
                        | _ => ERROR(":error:")
                        )
        in 
            case com of
            Quit => (h, [])
            | Push => let_(h, command(com, elem, i::j, []), tof)
            | Let =>
                let
                    val stack = let_(h, ([], []), true)
                    val lines = #1 stack
                    val env = #2 stack
                    val top = List.nth(env, 0)
                in
                    let_(lines, (top::i::j, []), true)
                end
            | End => (h, i::j)
            | _ => let_(h, command(com, Nil, i::j, []), tof)
        end
    | let_(g::h, (i::j, k::l), tof) =
        let
            val line = removeChar(g, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push => evalElem(getVal(line))
                        | _ => ERROR(":error:")
                        )
        in
            case com of
            Quit => (h, i::j)
            | Push => let_(h, command(com, elem, i::j, k::l), tof)
            | Let => 
                let
                    val stack = let_(h, ([], k::l), true)
                    val lines = #1 stack
                    val env = #2 stack
                    val top = List.nth(env, 0)
                in
                    if tof then let_(lines, (top::i::j, k::l), true) else let_(lines, (top::i::j, []), true)
                end
            | Fun => 
                let 
                    val expr = evalExpr(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, h, [], k::l, false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => let_(lines, (ERROR(":error:")::i::j, k::l), tof)
                    | _ => let_(lines, (funct::i::j, funct::k::l), tof) 
                end
            | InOutFun =>
                let 
                    val expr = evalInOut(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, h, [], k::l, true)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => let_(lines, (ERROR(":error:")::i::j, k::l), tof)
                    | _ => let_(lines, (funct::i::j, funct::k::l), tof) 
                end
            | Call =>
                (case i of
                NAME n => 
                    (let 
                        val funct = lookup(n, i::j)
                    in
                        case funct of
                        FUNCT(name, arg, lines, binds) => 
                            if length j < 1 then let_(h, (ERROR(":error:")::i::j, k::l), tof) else
                            let
                                val param = List.nth(j, 0)
                                val funRes = call(arg, param, lines, ([], k::l))
                                val returnVal = #1 funRes
                                val binds = #2 funRes
                            in
                                case returnVal of
                                ERROR err => let_(h, (returnVal::i::j, k::l), tof)
                                | _ => let_(h, (returnVal::List.drop(j, 1), k::l), tof)
                            end
                        | INOUT(name, arg, lines) =>
                            if length j < 1 then let_(h, (ERROR(":error:")::i::j, k::l), tof) else
                            let
                                val param = List.nth(j, 0)
                                val funRes = call(arg, param, lines, ([], k::l))
                                val returnVal = #1 funRes
                                val binds = #2 funRes
                            in
                                case returnVal of
                                ERROR err => let_(h, (returnVal::i::j, binds), tof)
                                | _ => let_(h, (returnVal::List.drop(j, 1), binds), tof)
                            end
                        | _ => let_(h, (ERROR(":error:")::i::j, k::l), tof)
                    end)
                | _ => (h, ERROR(":error:")::i::j))
            | End => (h, i::j)
            | _ => let_(h, command(com, Nil, i::j, k::l), tof)
        end
    | let_(_) = ([], [])

fun read([], ([], []), boo) = []
    | read(a::b, ([], []), boo) = 
        let
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                            Push => 
                            let 
                                val e = getVal(line)
                            in
                                evalElem(e)
                            end
                            | _ => STR("F")
                        )
        in 
            case com of 
            Quit => []
            | Push => read(b, command(com, elem, [], []), boo)
            | Let => 
                let
                    val l = let_(b, ([], []), true)
                    val lines = #1 l
                    val stack = #2 l
                    val top = List.nth(stack, 0)
                in
                    read(lines, (top::[], []), boo)
                end
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, b, [], [], false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR e => ERROR(":error:")::[]
                    | _ => read(lines, (funct::[], [funct]), boo) 
                end
            | InOutFun =>
                let 
                    val expr = evalInOut(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, b, [], [], true)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR e => ERROR(":error:")::[]
                    | INOUT(x, y, z) => read(lines, (funct::[], [funct]), boo)
                    | _ => read(lines, (funct::[], [funct]), boo)
                end
            | _ => read(b, command(com, Nil, [], []), boo)
        end
    | read(a::b, (c::d, []), boo) =
        let 
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push =>
                            let
                                val e = getVal(line)
                            in
                                evalElem(e)
                            end
                        | _ => ERROR(":error:")
                        )
        in
            case com of
            Quit => c::d
            | Push => read(b, command(com, elem, c::d, []), boo)
            | Let => 
                let
                    val l = let_(b, ([], []), true)
                    val lines = #1 l
                    val stack = #2 l
                    val top = List.nth(stack, 0)
                in
                    read(lines, (top::c::d, []), boo)
                end
            | Fun => 
                let 
                    val expr = evalExpr(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, b, [], [], false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR e => ERROR(":error:")::c::d
                    | _ => read(lines, (funct::c::d, [funct]), boo) 
                end
            | InOutFun =>
                let 
                    val expr = evalInOut(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, b, [], [], true)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR e => ERROR(":error:")::c::d
                    | _ => read(lines, (funct::c::d, [funct]), boo)
                end
            | _ => read(b, command(com, Nil, c::d, []), boo)
        end
    | read(a::b, ([], e::f), boo) =
        let 
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push =>
                            let
                                val e = getVal(line)
                            in
                                evalElem(e)
                            end
                        | _ => ERROR(":error:")
                        )
        in
            case com of
            Quit => []
            | Push => read(b, command(com, elem, [], e::f), boo)
            | Let => 
                let
                    val l = let_(b, ([], e::f), true)
                    val lines = #1 l
                    val stack = #2 l
                    val top = List.nth(stack, 0)
                in
                    read(lines, ([top], e::f), boo)
                end
            | _ => read(b, command(com, Nil, [], e::f), boo)
        end
    | read(a::b, (c::d, e::f), boo) =
        let 
            val line = removeChar(a, #"\n")
            val com = evalCommand(getCom(line))
            val elem = (case com of
                        Push =>
                            let
                                val e = getVal(line)
                            in
                                evalElem(e)
                            end
                        | _ => ERROR(":error:")
                        )
        in
            (case com of
            Quit => c::d
            | Push => read(b, command(com, elem, c::d, e::f), boo)
            | Let => 
                let
                    val l = let_(b, ([], e::f), true)
                    val lines = #1 l
                    val stack = #2 l
                    val top = List.nth(stack, 0)
                in
                    read(lines, (top::c::d, e::f), boo)
                end
            | Fun =>
                let 
                    val expr = evalExpr(line)
                    val name = #1 expr
                    val arg = #2 expr
                    val funRes = fun_(name, arg, b, [], e::f, false)
                    val lines = #1 funRes
                    val funct = #2 funRes
                in
                    case funct of
                    ERROR err => read(lines, (ERROR(":error:")::c::d, e::f), boo)
                    | _ => read(lines, (funct::c::d, funct::e::f), boo) 
                end
            | Call =>
                (case c of
                NAME n => 
                    (let 
                        val funct = lookup(n, e::f)
                    in
                        case funct of
                        FUNCT(name, arg, lines, binds) => 
                            if length d < 1 then read(b, (ERROR(":error:")::c::d, e::f), boo) else
                            let
                                val param = List.nth(d, 0)
                                val funRes = 
                                (case param of 
                                NAME q => 
                                    let 
                                        val value = lookup(q, e::f)
                                    in
                                        case value of
                                        ERROR err => call(arg, param, lines, ([], e::f))
                                        | FUNCT(w, x, y, z) => call(arg, param, lines, ([], e::f))
                                        | _ => call(arg, value, lines, ([], e::f))
                                    end
                                | _ => call(arg, param, lines, ([], e::f))
                                )
                                val returnVal = #1 funRes
                                val binds = #2 funRes
                            in
                                case returnVal of
                                ERROR err => read(b, (returnVal::c::d, e::f), boo)
                                | _ => read(b, (returnVal::List.drop(d, 1), e::f), boo)
                            end
                        | INOUT(name, arg, lines) =>
                            if length d < 1 then read(b, (ERROR(":error:")::c::d, e::f), boo) else
                            let
                                val param = List.nth(d, 0)
                                val funRes = 
                                (case param of 
                                NAME q => 
                                    let 
                                        val value = lookup(q, e::f)
                                    in
                                        case value of
                                        ERROR err => call(arg, param, lines, ([], e::f))
                                        | _ => call(arg, param, lines, ([], e::f))
                                    end
                                | _ => call(arg, param, lines, ([], e::f))
                                )
                                val returnVal = #1 funRes
                                val binds = #2 funRes
                            in
                                case returnVal of
                                ERROR err => read(b, (returnVal::c::d, binds), boo)
                                | Nil => read(b, (List.drop(d, 1), binds), boo)
                                | NAME nam =>
                                    let
                                        val lu = lookup(nam, binds)
                                    in
                                        case lu of
                                        ERROR err => read(b, (returnVal::List.drop(d, 1), e::f), boo)
                                        | _ => read(b, (lu::List.drop(d, 1), binds), boo)
                                    end
                                | _ => read(b, (returnVal::List.drop(d, 1), binds), boo)
                            end
                        | _ => read(b, (ERROR(":error:")::c::d, e::f), boo)
                    end)
                | _ => (ERROR(":error:")::c::d))
            | _ => read(b, command(com, Nil, c::d, e::f), boo))
        end
    | read(_) = []

fun interpreter(inFile : string, outFile : string) =
    let
        val l : string list = get(inFile)
    in
        output((read(l, ([], []), false)), outFile);
        ()
    end

fun test() = interpreter("input.txt", "output.txt")

fun sample(i : int) =
    let
        val input = "input_" ^ intToString i ^ ".txt"
    in
        interpreter(input, "output.txt")
    end