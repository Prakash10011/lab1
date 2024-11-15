let power exponent value = value ** float exponent
let square = power 2
let cube = power 3
printfn "Square of 4: %f" (square 4.0)  
printfn "Cube of 2: %f" (cube 2.0)   


let rec productOfListTail list acc =
    match list with
    | [] -> acc
    | head::tail -> productOfListTail tail (acc * head)
let numbers2 = [1; 2; 3; 4; 5]
let result = productOfListTail numbers2 1
printfn "Product of all elements: %d" result  


let rec productOfOdds n acc =
    if n <= 1 then acc
    else productOfOdds (n - 2) (acc * n)

// Usage
let oddProduct = productOfOdds 11 1
printfn "Product of odd numbers down to 1: %d" oddProduct  


let names = [" Charles"; "Babbage  "; "  Von Neumann  "; "  Dennis Ritchie  "]
let trimmedNames = names |> List.map (fun name -> name.Trim())
printfn "Trimmed Names: %A" trimmedNames


let numbers = [1..700]
let filteredSum = 
    numbers
    |> List.filter (fun n -> n % 5 = 0 && n % 7 = 0)
    |> List.sum
printfn "Sum of multiples of 5 and 7: %d" filteredSum


let names2 = ["James"; "Robert"; "John"; "William"; "Michael"; "David"; "Richard"]
let concatenatedNames = 
    names2 
    |> List.filter (fun name -> name.ToLower().Contains("i"))
    |> List.fold (fun acc name -> acc + name) ""

printfn "Concatenated Names: %s" concatenatedNames 
