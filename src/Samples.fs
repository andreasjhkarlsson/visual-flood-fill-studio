module Samples

open System.Drawing


let loadFromFile (filename: string) =
    let bitmap = new Bitmap(filename)
    [0..(bitmap.Width-1)] |> List.collect (fun x ->
        [0..(bitmap.Height-1)] |> List.map (fun y -> x,y)
    )
    |> List.map (fun (x,y) ->
        let color = bitmap.GetPixel(x,y)
        if (((int color.B) + (int color.G) + (int color.R)) / 3) < 127 then "1" else "0"
    )   
    |> String.concat ""

let rays = loadFromFile @"F:\dev\Postcard\images\rays.png"

let egypt = loadFromFile @"F:\dev\Postcard\images\egypt.png"

let atlas = loadFromFile @"F:\dev\Postcard\images\atlas.png"

let cat = loadFromFile @"F:\dev\Postcard\images\cat.png"

let maze = loadFromFile @"F:\dev\Postcard\images\maze.png"

let bubbles = loadFromFile @"F:\dev\Postcard\images\bubbles.png"