let rec isMember (atom : string) (l_input : string list) =
    if l_input.IsEmpty then false
    else (l_input.Head = atom) || isMember atom l_input.Tail

let rec rember (atom : string) (atomicList: string list) =
    if atomicList.IsEmpty then []
    elif atomicList.Head = atom then atomicList.Tail
    else atomicList.Head :: rember atom atomicList.Tail

let rec firsts (atomicList: List<string list>) =
    if atomicList.IsEmpty then []
    else atomicList.Head.Head :: firsts atomicList.Tail

let rec insertR (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = oldElem 
            then oldElem :: newElem :: atomicList.Tail
        else 
            atomicList.Head :: insertR newElem oldElem atomicList.Tail

let rec insertL (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = oldElem 
            then newElem :: atomicList
        else
            atomicList.Head :: insertL newElem oldElem atomicList.Tail        

let rec subst (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = oldElem
            then newElem :: atomicList.Tail
        else
            atomicList.Head :: subst newElem oldElem atomicList.Tail

let rec subst2 (newElem : string) (old1 : string) (old2 : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = old1 || atomicList.Head = old2
            then newElem :: atomicList.Tail
        else
            atomicList.Head :: subst2 newElem old1 old2 atomicList.Tail

let rec multirember (atom : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = atom
            then multirember atom atomicList.Tail
        else
            atomicList.Head :: multirember atom atomicList.Tail

let rec multiInsertR (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then[]
    else
        if oldElem = atomicList.Head
            then oldElem :: newElem :: multiInsertR newElem oldElem atomicList.Tail
        else
            atomicList.Head :: multiInsertR newElem oldElem atomicList.Tail

let rec multiInsertL (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = oldElem
            then newElem :: oldElem :: multiInsertL newElem oldElem atomicList.Tail
        else
            atomicList.Head :: multiInsertL newElem oldElem atomicList.Tail

let rec multisubst (newElem : string) (oldElem : string) (atomicList : string list) =
    if atomicList.IsEmpty then []
    else
        if atomicList.Head = oldElem
            then newElem :: multisubst newElem oldElem atomicList.Tail
        else
            atomicList.Head :: multisubst newElem oldElem atomicList.Tail

let add1 (num : uint32) =
    num + 1u

let sub1 (num : uint32) =
    num - 1u

let isZero (num : uint32) =
    num = 0u

let rec recAdd (num1 : uint32) (num2 : uint32) =
    if isZero num2 then num1
    else 
        add1 (recAdd num1 (sub1 num2)) 
        // sub1 num2 |> recAdd num1 |> add1

let rec recSub (num1 : uint32) (num2 : uint32) =
    if isZero num2 then num1
    else
        sub1 (recSub num1 (sub1 num2))
        // sub1 num2 |> recSub num1 |> sub1

let rec addTup (tup : uint32 list) =
    if tup.IsEmpty then 0u
    else
        recAdd tup.Head (addTup tup.Tail)
        //addTup tup.Tail |> recAdd tup.Head

let rec recMultiply (num1 : uint32) (num2 : uint32) =
    if isZero num2 then 0u
    else
        recAdd num1 (recMultiply num1 (sub1 num2))
        // sub1 num2 |> recMultiply num1 |> recAdd num1

let rec tupAddition (tup1 : uint32 list) (tup2: uint32 list) =
    if tup1.IsEmpty then tup2
    elif tup2.IsEmpty then tup1
    else
        recAdd tup1.Head tup2.Head :: tupAddition tup1.Tail tup2.Tail

let rec greaterThan (num1 : uint32) (num2 : uint32) =
    if isZero num1 then false
    elif isZero num2 then true
    else
        greaterThan (sub1 num1) (sub1 num2)

let rec lessThan (num1 : uint32) (num2 : uint32) =
    if isZero num2 then false
    elif isZero num1 then true
    else
        lessThan (sub1 num1) (sub1 num2)


let rec isEqual (num1 : uint32) (num2 : uint32) =
    // if greaterThan num1 num2 || lessThan num1 num2
    //     then false
    // else
    //     true
    not (greaterThan num1 num2 || lessThan num1 num2)

let rec recPow (baseNum : uint32) (expNum : uint32) =
    if isZero expNum then 1u
    else
        // recMultiply baseNum (recPow baseNum (sub1 expNum))
        sub1 expNum |> recPow baseNum |> recMultiply baseNum

let rec recDiv (num1 : uint32) (num2 : uint32) =
    if lessThan num1 num2 then 0u
    else
        // add1 (recDiv (recSub num1 num2) num2)
        recDiv (recSub num1 num2) num2 |> add1

