[<FunScript.JS>]
module Program

open System
open System.IO
open System.Diagnostics
open FunScript.Compiler
open FunScript.TypeScript

[<ReflectedDefinition>]
module ClientApplication =

    type Mode = Fill | Erase | Flood

    type Pixel = Filled | Cleared

    type IPixelBox =
        abstract Item: int*int -> Pixel with get, set

        abstract Width: int
        abstract Height: int

    type Recorder(source: IPixelBox) =
        let mutable pixels =
            [0..(source.Width-1)] |> List.collect (fun x ->
                [0..(source.Height-1)] |> List.map (fun y -> x,y)
            )
            |> List.map (fun (x,y) -> ((x,y), source.[x,y]))
            |> Map.ofList

        let mutable history: ((int*int)*Pixel) list = []

        member this.Playback speed =
            
            let delay (fn: unit -> unit) = Globals.window.setTimeout(unbox<Function> fn,speed) |> ignore

            let rec playback history () =
                match history with
                | ((x,y),state)::rest ->
                    source.[x,y] <- state
                    playback rest |> delay
                | [] -> ()

            do history |> List.rev |> playback |> delay

        interface IPixelBox with
            member this.Width = source.Width
            member this.Height = source.Height
            member this.Item
                with get (x,y) = pixels |> Map.find (x,y)
                and set (x,y) state =
                    pixels <- pixels |> Map.add (x,y) state
                    history <- ((x,y), state) :: history

    type HtmlPixelBox(element: Element) =

        let ``$`` = Globals.Dollar
 
        let width = ``$``.Invoke(element).data("width") |> unbox<int>
        let height = ``$``.Invoke(element).data("height") |> unbox<int>

        let td x y =
            ``$``.Invoke("tr",element).eq(float <| y).find("td").eq(float <| x)

        do
            for y in {1..height} do
                let row = ``$``.Invoke("<tr></tr>")
                for x in {1..width} do
                    let cell = ``$``.Invoke("<td></td>")
                    do row.append(cell) |> ignore
                    do cell.data("x",x-1) |> ignore
                    do cell.data("y",y-1) |> ignore
                (``$``.Invoke element).append row |> ignore   

        member this.Table = element

        member this.CoordinatesFromElement (element: Element) =
            let element = ``$``.Invoke(element)
            element.data "x" |> unbox<int>, element.data "y" |> unbox<int>

        interface IPixelBox with
            member this.Width = width
            member this.Height = height

            member this.Item
                with get (x,y) = if (td x y).hasClass "filled" then Filled else Cleared
                and set (x,y) state =
                    let td = td x y
                    let currentlyFilled = td.hasClass "filled"
                    match state with
                    | Filled when not currentlyFilled ->
                        td.addClass "filled" |> ignore
                    | Cleared when currentlyFilled ->
                        td.removeClass "filled" |> ignore
                    | _ -> () // State is already correct

    let main () = 
        let ``$`` = Globals.Dollar

        let select (selector: string) = ``$``.Invoke selector

        ``$``.Invoke(unbox<Function> (fun () ->

            let screen = HtmlPixelBox(select "#main-pixel-box" :?> Element)

            let mode () =
                (select "#mode-select option:selected")._val ()
                |> unbox<string>
                |> function
                    | "fill" -> Fill
                    | "erase" -> Erase
                    | "flood" -> Flood
                    |_ -> failwith "Not implemented"
            
            let neighbours (pixelbox: IPixelBox) (x,y) =
                [(-1,0); (1,0); (0,-1); (0,1)]
                |> List.map (fun (dx,dy) -> x+dx,y+dy)
                |> List.filter (fun (x,y) -> x >= 0 && x < pixelbox.Width && y >= 0 && y < pixelbox.Height)

            let rec floodfill (pixelbox: IPixelBox) (x,y)=
                if pixelbox.[x,y] = Cleared then
                    do pixelbox.[x,y] <- Filled
                    neighbours pixelbox (x,y) |> List.iter (floodfill pixelbox)
                    
            let pixelActivated (x, y) =
                let pixelbox = screen :> IPixelBox
                match mode () with
                | Fill -> do pixelbox.[x,y] <- Filled
                | Erase -> do pixelbox.[x,y] <- Cleared
                | Flood ->
                    let recorder = Recorder(pixelbox)
                    do floodfill (recorder :> IPixelBox) (x,y)
                    do recorder.Playback ()

            do
            
                let mutable mouseDown = false

                let targetToCoordinates (target: EventTarget) = screen.CoordinatesFromElement (target :?> Element)

                ``$``.Invoke(screen.Table).on("mousedown","td",Func<JQueryEventObject,obj array,obj>(fun e _ ->
                    do e.target |> targetToCoordinates |> pixelActivated
                    do mouseDown <- true
                    null
                )) |> ignore

                ``$``.Invoke(screen.Table).on("mouseenter","td",Func<JQueryEventObject,obj array,obj>(fun e _ ->
                    if mouseDown then
                        do e.target |> targetToCoordinates |> pixelActivated
                    null
                )) |> ignore

                ``$``.Invoke(screen.Table).on("mouseup","td",Func<JQueryEventObject,obj array,obj>(fun e _ ->
                    do mouseDown <- false
                    null
                )) |> ignore
        ))

do
    let js = Compiler.Compile(<@ ClientApplication.main () @>, noReturn = true)
    File.WriteAllText("app.js", js)
    Process.Start("index.html") |> ignore