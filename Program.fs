open System.Collections.Generic;

type TrieNode =
    | SubNodes of char * bool * TrieNode list
    | Nil
    member this.Char = match this with | Nil -> ' '
                                       | SubNodes(c,weh,subnodes) -> c

//    let list1d = [1; 3; 7; 9; 11; 13; 15; 19; 22; 29; 36]
//    let isEven x = x % 2 = 0
//    match List.tryFind isEven list1d with
//    | Some value -> printfn "The first even value is %d." value
//    | None -> printfn "There is no even value in the list."

    member this.GetChild(c:char) = match this with  | Nil -> []
                                                    | SubNodes(c,weh,subnodes) ->  match List.tryFind (fun (this:TrieNode) -> this.Char = c) subnodes with
                                                                                    | Some value -> [value]
                                                                                    | None -> []
                                                    //List.partition(fun (this:TrieNode) -> this.Char = c) subnodes
                                                    //if subnodes.Length > 0 then [ (List.filter(fun (this:TrieNode) -> this.Char = c) subnodes).Head ] else []

    member this.AWordEndsHere = match this with | Nil -> false
                                                | SubNodes(c,weh,subnodes) -> weh          

module NodeFunctions = 
    let rec nextLevel(node:TrieNode,curlevel:int) = match node with
                        | Nil -> curlevel
                        | SubNodes(_,_,subnodes) -> 
                            List.fold (fun acc (node:TrieNode) -> 
                                let res = nextLevel(node,curlevel+1)
                                if( acc > res) then acc else res) curlevel subnodes


module TrieFunctions = 
    let rec insertWord (wordChars:char list) = function
        //| Nil -> SubNodes(' ', false, (insertWord wordChars Nil)::[])
        | Nil -> SubNodes(wordChars.Head, false, (insertWord wordChars Nil)::[])
        //| SubNodes(_,_,[]) -> SubNodes(wordChars.Head,false,(insertWord wordChars.Tail Nil)::[] )
        //daca aici inca nu e cel putin un nod atunci fa nodul radacina standard adica nodul care nu e sfarsit de cuvant si
        //are caracterul ' ' si incepe sa ii construiesti lui subnodurile
        | SubNodes(c, weh, subnodes) as node ->
            
//            if(wordChars.Length = 1) then
//                SubNodes(wordChars.Head,true,[])
//            else
            if(wordChars.Length > 1) then
                let child = node.GetChild(wordChars.Head)
                if child = [] then 
                    SubNodes(wordChars.Head,false,(insertWord wordChars.Tail node)::subnodes )
                    //SubNodes(node.Char,false,(insertWord wordChars node)::subnodes )
                else
                    SubNodes(wordChars.Head,false,(insertWord wordChars.Tail child.Head)::subnodes )
            else
                SubNodes(wordChars.Head,true,[])
    let stringToCharList(s:string) = List.ofSeq s 


type Trie(inner : TrieNode) =
    member this.InsertWord(wordChars:char list) = Trie(TrieFunctions.insertWord wordChars inner)
    member this.InsertWord(str:string) = Trie(TrieFunctions.insertWord (TrieFunctions.stringToCharList str) inner)
    member this.Root() = inner
    //member this.Height() = NodeFunctions.nextLevel(inner,0)
 
let trie = Trie(SubNodes(' ',false,List.empty))
                .InsertWord("abc")
                .InsertWord("abcd")
                .InsertWord("abcd")
                .InsertWord("abcde")
                .InsertWord("abcdef")
                .InsertWord("ab123cd")
                .InsertWord("abc123d")
                .InsertWord("abc132d")

printf "%d\n" (NodeFunctions.nextLevel(trie.Root(),0))

