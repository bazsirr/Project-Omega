module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser
open Browser.Types

type Subject = { Id: int; Name: string; Color: string }

type Session = { Id: int; SubjectId: int; Minutes: int; Notes: string }

type Page = Dashboard | AddSession | History

type Model =
    { Page: Page
      Subjects: Subject list
      Sessions: Session list
      NewSubject: string
      NewMinutes: string
      NewNotes: string
      SelectedSubject: int option
      NextId: int }

type Msg =
    | GoTo of Page
    | SetNewSubject of string
    | AddSubject
    | SetMinutes of string
    | SetNotes of string
    | SetSubject of string
    | SubmitSession
    | DeleteSession of int

let init () =
    { Page = Dashboard
      Subjects =
          [ { Id = 1; Name = "Mathematics"; Color = "#3b82f6" }
            { Id = 2; Name = "Physics"; Color = "#ef4444" }
            { Id = 3; Name = "Computer Science"; Color = "#22c55e" } ]
      Sessions =
          [ { Id = 1; SubjectId = 1; Minutes = 45; Notes = "Derivatives" }
            { Id = 2; SubjectId = 3; Minutes = 60; Notes = "Data structures" }
            { Id = 3; SubjectId = 2; Minutes = 30; Notes = "Mechanics" } ]
      NewSubject = ""
      NewMinutes = "30"
      NewNotes = ""
      SelectedSubject = Some 1
      NextId = 10 }, Cmd.none

let update msg model =
    match msg with
    | GoTo p -> { model with Page = p }, Cmd.none
    | SetNewSubject v -> { model with NewSubject = v }, Cmd.none
    | AddSubject ->
        if model.NewSubject.Trim() = "" then model, Cmd.none
        else
            let colors = [| "#3b82f6"; "#ef4444"; "#22c55e"; "#f59e0b"; "#8b5cf6"; "#ec4899" |]
            let color = colors.[model.Subjects.Length % colors.Length]
            let s = { Id = model.NextId; Name = model.NewSubject.Trim(); Color = color }
            { model with Subjects = model.Subjects @ [s]; NextId = model.NextId + 1; NewSubject = "" }, Cmd.none
    | SetMinutes v -> { model with NewMinutes = v }, Cmd.none
    | SetNotes v -> { model with NewNotes = v }, Cmd.none
    | SetSubject v ->
        let sid = if v = "" then None else Some (int v)
        { model with SelectedSubject = sid }, Cmd.none
    | SubmitSession ->
        match model.SelectedSubject with
        | None -> model, Cmd.none
        | Some sid ->
            let mins = model.NewMinutes |> System.Int32.TryParse |> function (true, v) -> v | _ -> 0
            if mins <= 0 then model, Cmd.none
            else
                let s = { Id = model.NextId; SubjectId = sid; Minutes = mins; Notes = model.NewNotes }
                { model with
                    Sessions = s :: model.Sessions
                    NextId = model.NextId + 1
                    NewNotes = ""
                    Page = Dashboard }, Cmd.none
    | DeleteSession id ->
        { model with Sessions = model.Sessions |> List.filter (fun s -> s.Id <> id) }, Cmd.none

let findSubject subjects id =
    subjects |> List.tryFind (fun s -> s.Id = id)

let totalMinutes sessions =
    sessions |> List.sumBy (fun s -> s.Minutes)

let navbar model dispatch =
    nav [ Style [ Background "#111827"; Padding "16px 24px"; Display DisplayOptions.Flex
                  AlignItems "center"; JustifyContent "space-between"
                  BorderBottom "1px solid #1f2937" ] ] [
        span [ Style [ FontSize "20px"; FontWeight "800"; Color "#fff";
                       FontFamily "sans-serif"; LetterSpacing "-0.5px" ] ] [ str "◈ StudyFlow" ]
        div [ Style [ Display DisplayOptions.Flex; Gap "8px" ] ] [
            for (page, label) in [(Dashboard, "Dashboard"); (AddSession, "+ Log Session"); (History, "History")] do
                button
                    [ Style [ Background (if model.Page = page then "#6366f1" else "transparent")
                              Color (if model.Page = page then "#fff" else "#9ca3af")
                              Border (if model.Page = page then "none" else "1px solid #374151")
                              Padding "8px 16px"; BorderRadius "8px"; Cursor "pointer"
                              FontSize "13px"; FontWeight "600" ]
                      OnClick (fun _ -> dispatch (GoTo page)) ] [ str label ]
        ]
    ]

let statCard label value color =
    div [ Style [ Background "#1f2937"; Border "1px solid #374151"; BorderRadius "12px"
                  Padding "20px"; BorderLeft (sprintf "3px solid %s" color) ] ] [
        div [ Style [ FontSize "28px"; FontWeight "800"; Color "#fff"; FontFamily "sans-serif" ] ] [ str value ]
        div [ Style [ FontSize "12px"; Color "#9ca3af"; MarginTop "4px"; TextTransform "uppercase"; LetterSpacing "0.5px" ] ] [ str label ]
    ]

let dashboardPage model dispatch =
    let total = totalMinutes model.Sessions
    let hours = total / 60
    let mins = total % 60
    div [ Style [ Padding "32px 40px" ] ] [
        h1 [ Style [ FontSize "26px"; FontWeight "800"; Color "#fff"; MarginBottom "8px"; FontFamily "sans-serif" ] ] [ str "Dashboard" ]
        p [ Style [ Color "#6b7280"; MarginBottom "28px"; FontSize "14px" ] ] [
            str (sprintf "You have %d subjects and %d sessions logged." model.Subjects.Length model.Sessions.Length)
        ]

        div [ Style [ Display DisplayOptions.Grid; Gap "16px"; MarginBottom "32px"
                      CSSProp.Custom("grid-template-columns", "repeat(3, 1fr)") ] ] [
            statCard "Total Study Time" (sprintf "%dh %dm" hours mins) "#6366f1"
            statCard "Sessions Logged" (string model.Sessions.Length) "#22c55e"
            statCard "Active Subjects" (string model.Subjects.Length) "#f59e0b"
        ]

        div [ Style [ Background "#1f2937"; Border "1px solid #374151"; BorderRadius "12px"; Padding "24px"; MarginBottom "24px" ] ] [
            h2 [ Style [ FontSize "16px"; FontWeight "700"; Color "#fff"; MarginBottom "20px"; FontFamily "sans-serif" ] ] [ str "Time per Subject" ]
            div [ Style [ Display DisplayOptions.Flex; FlexDirection "column"; Gap "14px" ] ] [
                for sub in model.Subjects do
                    let subMins = model.Sessions |> List.filter (fun s -> s.SubjectId = sub.Id) |> List.sumBy (fun s -> s.Minutes)
                    let pct = if total > 0 then subMins * 100 / total else 0
                    div [] [
                        div [ Style [ Display DisplayOptions.Flex; JustifyContent "space-between"; MarginBottom "6px" ] ] [
                            span [ Style [ Color "#e5e7eb"; FontSize "13px"; FontWeight "500" ] ] [ str sub.Name ]
                            span [ Style [ Color "#9ca3af"; FontSize "12px" ] ] [ str (sprintf "%d min" subMins) ]
                        ]
                        div [ Style [ Background "#374151"; BorderRadius "4px"; Height "6px" ] ] [
                            div [ Style [ Background sub.Color; Width (sprintf "%d%%" pct); Height "6px"; BorderRadius "4px" ] ] []
                        ]
                    ]
            ]
        ]

        div [ Style [ Background "#1f2937"; Border "1px solid #374151"; BorderRadius "12px"; Padding "24px" ] ] [
            h2 [ Style [ FontSize "16px"; FontWeight "700"; Color "#fff"; MarginBottom "16px"; FontFamily "sans-serif" ] ] [ str "Add Subject" ]
            div [ Style [ Display DisplayOptions.Flex; Gap "10px" ] ] [
                input [ Style [ Background "#111827"; Border "1px solid #374151"; Color "#fff"; Padding "10px 14px"
                                BorderRadius "8px"; FontSize "14px"; Flex "1" ]
                        Placeholder "Subject name..."
                        Value model.NewSubject
                        OnChange (fun e -> dispatch (SetNewSubject e.Value)) ]
                button [ Style [ Background "#6366f1"; Color "#fff"; Border "none"; Padding "10px 20px"
                                 BorderRadius "8px"; FontSize "13px"; FontWeight "600"; Cursor "pointer" ]
                         OnClick (fun _ -> dispatch AddSubject) ] [ str "Add" ]
            ]
        ]
    ]

let addSessionPage model dispatch =
    div [ Style [ Padding "32px 40px"; MaxWidth "480px" ] ] [
        h1 [ Style [ FontSize "26px"; FontWeight "800"; Color "#fff"; MarginBottom "28px"; FontFamily "sans-serif" ] ] [ str "Log Study Session" ]
        div [ Style [ Background "#1f2937"; Border "1px solid #374151"; BorderRadius "12px"; Padding "28px" ] ] [
            div [ Style [ MarginBottom "18px" ] ] [
                label [ Style [ Display DisplayOptions.Block; FontSize "12px"; FontWeight "600"; Color "#9ca3af"
                                MarginBottom "6px"; TextTransform "uppercase"; LetterSpacing "0.5px" ] ] [ str "Subject" ]
                select [ Style [ Width "100%"; Background "#111827"; Border "1px solid #374151"; Color "#fff"
                                 Padding "10px 14px"; BorderRadius "8px"; FontSize "14px" ]
                         OnChange (fun e -> dispatch (SetSubject (e.target :?> HTMLSelectElement).value)) ] [
                    for sub in model.Subjects do
                        option [ Value (string sub.Id)
                                 Selected (model.SelectedSubject = Some sub.Id) ] [ str sub.Name ]
                ]
            ]
            div [ Style [ MarginBottom "18px" ] ] [
                label [ Style [ Display DisplayOptions.Block; FontSize "12px"; FontWeight "600"; Color "#9ca3af"
                                MarginBottom "6px"; TextTransform "uppercase"; LetterSpacing "0.5px" ] ] [ str "Duration (minutes)" ]
                input [ Style [ Width "100%"; Background "#111827"; Border "1px solid #374151"; Color "#fff"
                                Padding "10px 14px"; BorderRadius "8px"; FontSize "14px" ]
                        Type "number"; Value model.NewMinutes
                        OnChange (fun e -> dispatch (SetMinutes e.Value)) ]
            ]
            div [ Style [ MarginBottom "24px" ] ] [
                label [ Style [ Display DisplayOptions.Block; FontSize "12px"; FontWeight "600"; Color "#9ca3af"
                                MarginBottom "6px"; TextTransform "uppercase"; LetterSpacing "0.5px" ] ] [ str "Notes (optional)" ]
                textarea [ Style [ Width "100%"; Background "#111827"; Border "1px solid #374151"; Color "#fff"
                                   Padding "10px 14px"; BorderRadius "8px"; FontSize "14px"; MinHeight "80px" ]
                           Placeholder "What did you study?"
                           OnChange (fun e -> dispatch (SetNotes e.Value)) ] [ str model.NewNotes ]
            ]
            button [ Style [ Width "100%"; Background "#6366f1"; Color "#fff"; Border "none"; Padding "12px"
                             BorderRadius "8px"; FontSize "14px"; FontWeight "600"; Cursor "pointer" ]
                     OnClick (fun _ -> dispatch SubmitSession) ] [ str "Log Session" ]
        ]
    ]

let historyPage model dispatch =
    div [ Style [ Padding "32px 40px" ] ] [
        h1 [ Style [ FontSize "26px"; FontWeight "800"; Color "#fff"; MarginBottom "28px"; FontFamily "sans-serif" ] ] [ str "Session History" ]
        div [ Style [ Display DisplayOptions.Flex; FlexDirection "column"; Gap "12px" ] ] [
            if model.Sessions.IsEmpty then
                p [ Style [ Color "#6b7280" ] ] [ str "No sessions yet. Log your first session!" ]
            for s in model.Sessions do
                match findSubject model.Subjects s.SubjectId with
                | None -> ()
                | Some sub ->
                    div [ Style [ Background "#1f2937"; Border "1px solid #374151"; BorderRadius "10px"
                                  Padding "16px 20px"; Display DisplayOptions.Flex
                                  AlignItems "center"; Gap "14px" ] ] [
                        div [ Style [ Width "10px"; Height "10px"; BorderRadius "50%"; Background sub.Color; FlexShrink "0" ] ] []
                        div [ Style [ Flex "1" ] ] [
                            div [ Style [ FontWeight "600"; Color "#e5e7eb"; FontSize "14px" ] ] [ str sub.Name ]
                            if s.Notes <> "" then
                                div [ Style [ Color "#9ca3af"; FontSize "12px"; MarginTop "2px" ] ] [ str s.Notes ]
                        ]
                        span [ Style [ FontWeight "700"; Color "#fff"; FontSize "15px" ] ] [ str (sprintf "%d min" s.Minutes) ]
                        button [ Style [ Background "transparent"; Border "none"; Color "#6b7280"; FontSize "18px"
                                         Cursor "pointer"; Padding "0 4px" ]
                                 OnClick (fun _ -> dispatch (DeleteSession s.Id)) ] [ str "×" ]
                    ]
        ]
    ]

let view model dispatch =
    div [ Style [ Background "#0f172a"; MinHeight "100vh"; FontFamily "'DM Sans', sans-serif" ] ] [
        navbar model dispatch
        match model.Page with
        | Dashboard -> dashboardPage model dispatch
        | AddSession -> addSessionPage model dispatch
        | History -> historyPage model dispatch
    ]

Program.mkProgram init update view
|> Program.withReactSynchronous "app-root"
|> Program.run
