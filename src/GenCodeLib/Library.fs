namespace GenCodeLib
module Encoder =
    open System
    type Codon = DataCodon of byte | StopCodon of byte

    let rec next_token (str: string) (at:int) : (char * int) option =
       
        if str.Length <= at then
            None
        else                 
           let _char = str.[at]
           if Char.IsWhiteSpace(_char) then
               next_token str (at + 1) 
           elif _char = '>' then
                let eol = str.IndexOf('\n', at)
                if eol = -1 then
                    None                    
                else
                    next_token str (eol + 1)                
            else     
                Some (_char, (at + 1))          
        
    module Test =     
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``retrieve tokens from string, ignoring whitespace and comments`` () = 
            
            let next1 = next_token "c G\na\n>BLABALA\nU"
            match (next1 0) with
               | Some (a, n) -> a |> should equal 'c', n |> should equal 1                
               | None -> failwith "Should not have terminated"
            |> ignore
                        
            match (next1 1)  with
               | Some (a, n) -> a |> should equal 'G', n |> should equal 3                
               | None -> failwith "Should not have terminated"   
            |> ignore
            
            match (next1 3) with
               | Some (a, n) -> a |> should equal 'a', n |> should equal 5                
               | None ->  failwith "Should not have terminated"
            |> ignore
                                  
            match (next1 5) with
               | Some (a, n) -> a |> should equal 'U', n |> should equal 16                 
               | None ->  failwith "Should not have terminated"
            |> ignore
            
            match (next1 16) with
               | Some (a, n) -> false            
               | None -> true            
            |> should equal true

        
         (*
        [<Test>]
        let ``get genes from multiline with comments``() = 
            
            let code = "uuucaugugcccaaaauccucucaggcauggucaagcccauccuuuucc
acaacacagccuag
>NM_001293063 1
augugcgaggacugcugugcugcaacuguuuuccguccuuucuuucacuaa"
            () //List.head getGenes code |> should equal codon(uuu)
                                  
           let c = Char.ToUpper input.[pos]
            //skip until end of line
            if skip then
                if Char.IsSeparator c then  
                    return nexttoken pos + 1, false, input
                else 
                    return nexttoken pos + 1, true, input
            else 
                if c == '>' then    
                    nexttoken pos + 1, true, input
                else 
                    return c*)