open System.Collections.Generic;

type TrieNode = TrieNode of (char * bool * TrieNode list) with
    member this.Char = match this with TrieNode(c,weh,subnodes) -> c

    member this.GetChild(c:char) = match this with TrieNode(c,weh,subnodes) ->  match List.tryFind (fun (this:TrieNode) -> this.Char = c) subnodes with
                                                                                    | Some value -> [value]
                                                                                    | None -> []

    member this.AWordEndsHere = match this with TrieNode(_,weh,_) -> weh          

    member this.Subnodes = match this with TrieNode(_,_,subnodes) -> subnodes         

module NodeFunctions = 
    let rec nextLevel(node:TrieNode,curlevel:int) = match node with TrieNode(_,_,subnodes) -> List.fold (fun acc (node:TrieNode) -> 
                                                                                                let res = nextLevel(node,curlevel+1)
                                                                                                if( acc > res) then acc else res) curlevel subnodes
                            

module TrieFunctions = 
    let rec insertWord (wordChars:char list) = function
        | TrieNode(c, weh, subnodes) as node ->
            if(wordChars.Length > 0) then
                let child = node.GetChild(wordChars.Head)
                if child = [] then 
                    let newnode = TrieNode(wordChars.Head,false,[])
                    TrieNode(c,weh,(insertWord wordChars.Tail newnode)::subnodes )
                else
                    TrieNode(wordChars.Head,false,(insertWord wordChars.Tail child.Head)::subnodes )
            else
                TrieNode(c,true,[])
    let stringToCharList(s:string) = List.ofSeq s 
    let rec print acc = function
    | TrieNode(c, weh, []) -> 
        let str = acc + c.ToString()
        if weh then printfn "%s" str
        ()
    | TrieNode(c, weh, subnodes) -> 
        let str = acc + (if c.Equals(' ') then "" else c.ToString())
        if weh then printfn "%s" str
        List.iter (fun (node : TrieNode) -> print str node) subnodes
            


type Trie(inner : TrieNode) =
    member this.InsertWord(wordChars:char list) = Trie(TrieFunctions.insertWord wordChars inner)
    member this.InsertWord(str:string) = Trie(TrieFunctions.insertWord (TrieFunctions.stringToCharList str) inner)
    member this.Root() = inner
    new() = Trie(TrieNode(' ',false,List.empty))
 
let trie = Trie()
                .InsertWord("abc")
                .InsertWord("abcd")
                .InsertWord("abcd")
                .InsertWord("abcde")
                .InsertWord("abcdef")
                .InsertWord("ab123cd")
                .InsertWord("abc123d")
                .InsertWord("abc132d")

TrieFunctions.print "" (trie.Root())

printf "%d\n" (NodeFunctions.nextLevel(trie.Root(),0))

