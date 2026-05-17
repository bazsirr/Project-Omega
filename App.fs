module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser
open Browser.Types

// ──────────────────────────────────────────────
// Domain Types
// ──────────────────────────────────────────────

type Subject =
    { Id: int
      Name: string
      Color: string
      WeeklyGoalMinutes: int }

type Task =
    { Id: int
      SubjectId: int
      Title: string
      Completed: bool
      CreatedAt: System.DateTime }

type PomodoroPhase =
    | Work
    | ShortBreak
    | LongBreak

type PomodoroState =
    | Idle
    | Running
    | Paused

type StudySession =
    { Id: int
      SubjectId: int
      Minutes: int
      Date: System.DateTime
      Notes: string }

type Page =
    | Dashboard
    | Pomodoro
    | Tasks
    | History

type Modal =
    | NoModal
    | AddSubjectModal
    | AddTaskModal
    | LogSessionModal

// ──────────────────────────────────────────────
// Model
// ──────────────────────────────────────────────

type Model =
    { // Navigation
      CurrentPage: Page

      // Data
      Subjects: Subject list
      Tasks: Task list
      Sessions: StudySession list

      // Pomodoro
      Phase: PomodoroPhase
      PomodoroStatus: PomodoroState
      SecondsLeft: int
      PomodoroCount: int
      ActiveSubjectId: int option

      // Forms
      Modal: Modal
      NewSubjectName: string
      NewSubjectGoal: string
      NewSubjectColor: string
      NewTaskTitle: string
      NewTaskSubjectId: int option
      LogMinutes: string
      LogSubjectId: int option
      LogNotes: string

      // UI
      NextId: int }

let pomoDuration = function
    | Work -> 25 * 60
    | ShortBreak -> 5 * 60
    | LongBreak -> 15 * 60

let subjectColors =
    [| "#e74c3c"; "#e67e22"; "#f1c40f"; "#2ecc71"
       "#1abc9c"; "#3498db"; "#9b59b6"; "#e91e63" |]

let sampleSubjects =
    [ { Id = 1; Name = "Mathematics"; Color = "#3498db"; WeeklyGoalMinutes = 120 }
      { Id = 2; Name = "Physics"; Color = "#e74c3c"; WeeklyGoalMinutes = 90 }
      { Id = 3; Name = "Computer Science"; Color = "#2ecc71"; WeeklyGoalMinutes = 180 } ]

let sampleTasks =
    [ { Id = 1; SubjectId = 1; Title = "Chapter 5 exercises"; Completed = false; CreatedAt = System.DateTime.Now }
      { Id = 2; SubjectId = 3; Title = "Implement sorting algorithm"; Completed = true; CreatedAt = System.DateTime.Now }
      { Id = 3; SubjectId = 2; Title = "Read thermodynamics notes"; Completed = false; CreatedAt = System.DateTime.Now } ]

let sampleSessions =
    let today = System.DateTime.Now
    [ { Id = 1; SubjectId = 1; Minutes = 45; Date = today.AddDays(-1.0); Notes = "Derivatives practice" }
      { Id = 2; SubjectId = 3; Minutes = 60; Date = today.AddDays(-2.0); Notes = "Data structures lecture" }
      { Id = 3; SubjectId = 2; Minutes = 30; Date = today.AddDays(-3.0); Notes = "Mechanics problems" }
      { Id = 4; SubjectId = 1; Minutes = 50; Date = today.AddDays(-5.0); Notes = "Integrals review" }
      { Id = 5; SubjectId = 3; Minutes = 90; Date = today.AddDays(-6.0); Notes = "Project work" } ]

let init () =
    { CurrentPage = Dashboard
      Subjects = sampleSubjects
      Tasks = sampleTasks
      Sessions = sampleSessions
      Phase = Work
      PomodoroStatus = Idle
      SecondsLeft = pomoDuration Work
      PomodoroCount = 0
      ActiveSubjectId = None
      Modal = NoModal
      NewSubjectName = ""
      NewSubjectGoal = "60"
      NewSubjectColor = subjectColors.[0]
      NewTaskTitle = ""
      NewTaskSubjectId = None
      LogMinutes = "25"
      LogSubjectId = None
      LogNotes = ""
      NextId = 10 }, Cmd.none

// ──────────────────────────────────────────────
// Messages
// ──────────────────────────────────────────────

type Msg =
    | NavigateTo of Page
    | OpenModal of Modal
    | CloseModal
    // Pomodoro
    | Tick
    | StartPomodoro
    | PausePomodoro
    | ResetPomodoro
    | SetPhase of PomodoroPhase
    | SetActiveSubject of int option
    // Subject form
    | UpdateNewSubjectName of string
    | UpdateNewSubjectGoal of string
    | UpdateNewSubjectColor of string
    | SubmitNewSubject
    // Task form
    | UpdateNewTaskTitle of string
    | UpdateNewTaskSubject of int option
    | SubmitNewTask
    | ToggleTask of int
    | DeleteTask of int
    // Session log
    | UpdateLogMinutes of string
    | UpdateLogSubjectId of int option
    | UpdateLogNotes of string
    | SubmitLogSession

// ──────────────────────────────────────────────
// Update
// ──────────────────────────────────────────────

let advancePhase model =
    let newCount = model.PomodoroCount + 1
    let nextPhase =
        if newCount % 8 = 0 then LongBreak
        elif newCount % 2 = 0 then ShortBreak
        else Work
    { model with
        Phase = nextPhase
        PomodoroCount = newCount
        SecondsLeft = pomoDuration nextPhase
        PomodoroStatus = Idle }

let update msg model =
    match msg with
    | NavigateTo page ->
        { model with CurrentPage = page }, Cmd.none

    | OpenModal m ->
        { model with Modal = m }, Cmd.none

    | CloseModal ->
        { model with Modal = NoModal }, Cmd.none

    | Tick ->
        if model.PomodoroStatus = Running then
            if model.SecondsLeft <= 1 then
                // Session completed — log it if subject selected
                let newSessions =
                    match model.ActiveSubjectId with
                    | Some sid when model.Phase = Work ->
                        let s = { Id = model.NextId; SubjectId = sid; Minutes = 25
                                  Date = System.DateTime.Now; Notes = "Pomodoro session" }
                        s :: model.Sessions
                    | _ -> model.Sessions
                let m2 = advancePhase { model with Sessions = newSessions; NextId = model.NextId + 1 }
                m2, Cmd.none
            else
                { model with SecondsLeft = model.SecondsLeft - 1 }, Cmd.none
        else
            model, Cmd.none

    | StartPomodoro ->
        { model with PomodoroStatus = Running }, Cmd.none

    | PausePomodoro ->
        { model with PomodoroStatus = Paused }, Cmd.none

    | ResetPomodoro ->
        { model with PomodoroStatus = Idle; SecondsLeft = pomoDuration model.Phase }, Cmd.none

    | SetPhase phase ->
        { model with Phase = phase; PomodoroStatus = Idle; SecondsLeft = pomoDuration phase }, Cmd.none

    | SetActiveSubject sid ->
        { model with ActiveSubjectId = sid }, Cmd.none

    | UpdateNewSubjectName v -> { model with NewSubjectName = v }, Cmd.none
    | UpdateNewSubjectGoal v -> { model with NewSubjectGoal = v }, Cmd.none
    | UpdateNewSubjectColor v -> { model with NewSubjectColor = v }, Cmd.none

    | SubmitNewSubject ->
        if model.NewSubjectName.Trim() = "" then model, Cmd.none
        else
            let goal = model.NewSubjectGoal |> System.Int32.TryParse |> function (true, v) -> v | _ -> 60
            let sub = { Id = model.NextId; Name = model.NewSubjectName.Trim()
                        Color = model.NewSubjectColor; WeeklyGoalMinutes = goal }
            { model with
                Subjects = model.Subjects @ [sub]
                NextId = model.NextId + 1
                Modal = NoModal
                NewSubjectName = "" }, Cmd.none

    | UpdateNewTaskTitle v -> { model with NewTaskTitle = v }, Cmd.none
    | UpdateNewTaskSubject v -> { model with NewTaskSubjectId = v }, Cmd.none

    | SubmitNewTask ->
        match model.NewTaskTitle.Trim(), model.NewTaskSubjectId with
        | "", _ | _, None -> model, Cmd.none
        | title, Some sid ->
            let t = { Id = model.NextId; SubjectId = sid; Title = title
                      Completed = false; CreatedAt = System.DateTime.Now }
            { model with
                Tasks = model.Tasks @ [t]
                NextId = model.NextId + 1
                Modal = NoModal
                NewTaskTitle = ""
                NewTaskSubjectId = None }, Cmd.none

    | ToggleTask id ->
        let tasks = model.Tasks |> List.map (fun t -> if t.Id = id then { t with Completed = not t.Completed } else t)
        { model with Tasks = tasks }, Cmd.none

    | DeleteTask id ->
        { model with Tasks = model.Tasks |> List.filter (fun t -> t.Id <> id) }, Cmd.none

    | UpdateLogMinutes v -> { model with LogMinutes = v }, Cmd.none
    | UpdateLogSubjectId v -> { model with LogSubjectId = v }, Cmd.none
    | UpdateLogNotes v -> { model with LogNotes = v }, Cmd.none

    | SubmitLogSession ->
        match model.LogSubjectId with
        | None -> model, Cmd.none
        | Some sid ->
            let mins = model.LogMinutes |> System.Int32.TryParse |> function (true, v) -> v | _ -> 0
            if mins <= 0 then model, Cmd.none
            else
                let s = { Id = model.NextId; SubjectId = sid; Minutes = mins
                          Date = System.DateTime.Now; Notes = model.LogNotes }
                { model with
                    Sessions = s :: model.Sessions
                    NextId = model.NextId + 1
                    Modal = NoModal
                    LogMinutes = "25"
                    LogNotes = ""
                    LogSubjectId = None }, Cmd.none

// ──────────────────────────────────────────────
// Helpers
// ──────────────────────────────────────────────

let findSubject (subjects: Subject list) id =
    subjects |> List.tryFind (fun s -> s.Id = id)

let totalMinutesThisWeek (sessions: StudySession list) subjectId =
    let weekAgo = System.DateTime.Now.AddDays(-7.0)
    sessions
    |> List.filter (fun s -> s.SubjectId = subjectId && s.Date >= weekAgo)
    |> List.sumBy (fun s -> s.Minutes)

let totalMinutesAllTime (sessions: StudySession list) =
    sessions |> List.sumBy (fun s -> s.Minutes)

let formatTime seconds =
    let m = seconds / 60
    let s = seconds % 60
    sprintf "%02d:%02d" m s

// ──────────────────────────────────────────────
// View Helpers
// ──────────────────────────────────────────────

let phaseLabel = function Work -> "Focus" | ShortBreak -> "Short Break" | LongBreak -> "Long Break"

// ──────────────────────────────────────────────
// View
// ──────────────────────────────────────────────

let navItem dispatch currentPage page label icon =
    let active = currentPage = page
    li [ Class (if active then "nav-item active" else "nav-item") ] [
        button [ OnClick (fun _ -> dispatch (NavigateTo page)) ] [
            span [ Class "nav-icon" ] [ str icon ]
            span [ Class "nav-label" ] [ str label ]
        ]
    ]

let statsCard title value subtitle color =
    div [ Class "stats-card"; Style [ BorderLeftColor color ] ] [
        div [ Class "stats-value" ] [ str value ]
        div [ Class "stats-title" ] [ str title ]
        div [ Class "stats-sub" ] [ str subtitle ]
    ]

let subjectBadge (sub: Subject) =
    span [ Class "subject-badge"; Style [ BackgroundColor sub.Color ] ] [ str sub.Name ]

// ── Dashboard Page ──

let dashboardPage model dispatch =
    let totalMins = totalMinutesAllTime model.Sessions
    let totalHours = totalMins / 60
    let totalRem = totalMins % 60
    let weekMins = 
        let weekAgo = System.DateTime.Now.AddDays(-7.0)
        model.Sessions |> List.filter (fun s -> s.Date >= weekAgo) |> List.sumBy (fun s -> s.Minutes)
    let completedTasks = model.Tasks |> List.filter (fun t -> t.Completed) |> List.length
    let pendingTasks = model.Tasks |> List.filter (fun t -> not t.Completed) |> List.length

    div [ Class "page" ] [
        div [ Class "page-header" ] [
            div [] [
                h1 [] [ str "Dashboard" ]
                p [ Class "subtitle" ] [ str (sprintf "Welcome back — %s" (System.DateTime.Now.ToString("dddd, MMMM d"))) ]
            ]
            button [ Class "btn-primary"; OnClick (fun _ -> dispatch (OpenModal LogSessionModal)) ] [
                str "+ Log Session"
            ]
        ]

        div [ Class "stats-grid" ] [
            statsCard "Total Study Time" (sprintf "%dh %dm" totalHours totalRem) "all time" "#3498db"
            statsCard "This Week" (sprintf "%d min" weekMins) "last 7 days" "#2ecc71"
            statsCard "Active Subjects" (string model.Subjects.Length) "subjects tracked" "#9b59b6"
            statsCard "Task Progress" (sprintf "%d / %d" completedTasks (completedTasks + pendingTasks)) "tasks completed" "#e67e22"
        ]

        div [ Class "dashboard-grid" ] [
            // Subject progress
            div [ Class "card" ] [
                div [ Class "card-header" ] [
                    h2 [] [ str "Weekly Goals" ]
                    button [ Class "btn-ghost"; OnClick (fun _ -> dispatch (OpenModal AddSubjectModal)) ] [ str "+ Subject" ]
                ]
                div [ Class "subject-list" ] [
                    for sub in model.Subjects do
                        let done_ = totalMinutesThisWeek model.Sessions sub.Id
                        let pct = if sub.WeeklyGoalMinutes > 0 then min 100 (done_ * 100 / sub.WeeklyGoalMinutes) else 0
                        div [ Class "subject-row" ] [
                            div [ Class "subject-row-header" ] [
                                span [ Class "dot"; Style [ BackgroundColor sub.Color ] ] []
                                span [ Class "subject-name" ] [ str sub.Name ]
                                span [ Class "subject-time" ] [ str (sprintf "%d / %d min" done_ sub.WeeklyGoalMinutes) ]
                            ]
                            div [ Class "progress-bar" ] [
                                div [ Class "progress-fill"
                                      Style [ Width (sprintf "%d%%" pct); BackgroundColor sub.Color ] ] []
                            ]
                        ]
                ]
            ]

            // Recent sessions
            div [ Class "card" ] [
                div [ Class "card-header" ] [
                    h2 [] [ str "Recent Sessions" ]
                ]
                div [ Class "session-list" ] [
                    for s in model.Sessions |> List.sortByDescending (fun s -> s.Date) |> List.truncate 5 do
                        match findSubject model.Subjects s.SubjectId with
                        | None -> ()
                        | Some sub ->
                            div [ Class "session-item" ] [
                                div [ Class "session-dot"; Style [ BackgroundColor sub.Color ] ] []
                                div [ Class "session-info" ] [
                                    span [ Class "session-subject" ] [ str sub.Name ]
                                    span [ Class "session-note" ] [ str s.Notes ]
                                ]
                                div [ Class "session-right" ] [
                                    span [ Class "session-mins" ] [ str (sprintf "%d min" s.Minutes) ]
                                    span [ Class "session-date" ] [ str (s.Date.ToString("MMM d")) ]
                                ]
                            ]
                ]
            ]

            // Pending tasks
            div [ Class "card" ] [
                div [ Class "card-header" ] [
                    h2 [] [ str "Upcoming Tasks" ]
                    button [ Class "btn-ghost"; OnClick (fun _ -> dispatch (OpenModal AddTaskModal)) ] [ str "+ Task" ]
                ]
                div [ Class "task-list" ] [
                    for t in model.Tasks |> List.filter (fun t -> not t.Completed) |> List.truncate 5 do
                        match findSubject model.Subjects t.SubjectId with
                        | None -> ()
                        | Some sub ->
                            div [ Class "task-item" ] [
                                button [ Class "task-check"; OnClick (fun _ -> dispatch (ToggleTask t.Id)) ] [ str "○" ]
                                span [ Class "task-title" ] [ str t.Title ]
                                subjectBadge sub
                            ]
                    if model.Tasks |> List.forall (fun t -> t.Completed) then
                        p [ Class "empty-state" ] [ str "All tasks done! 🎉" ]
                ]
            ]
        ]
    ]

// ── Pomodoro Page ──

let pomodoroPage model dispatch =
    let total = pomoDuration model.Phase
    let pct = float (total - model.SecondsLeft) / float total
    let radius = 90.0
    let circumference = 2.0 * System.Math.PI * radius
    let strokeDashoffset = circumference * (1.0 - pct)

    let phaseColor = function Work -> "#e74c3c" | ShortBreak -> "#2ecc71" | LongBreak -> "#3498db"
    let color = phaseColor model.Phase

    div [ Class "page" ] [
        div [ Class "page-header" ] [
            h1 [] [ str "Pomodoro Timer" ]
            p [ Class "subtitle" ] [ str (sprintf "Session %d completed" model.PomodoroCount) ]
        ]

        div [ Class "pomodoro-layout" ] [
            // Phase selector
            div [ Class "phase-tabs" ] [
                for phase in [Work; ShortBreak; LongBreak] do
                    button
                        [ Class (if model.Phase = phase then "phase-tab active" else "phase-tab")
                          OnClick (fun _ -> dispatch (SetPhase phase)) ] [
                        str (phaseLabel phase)
                    ]
            ]

            // Timer circle
            div [ Class "timer-container" ] [
                svg [ HTMLAttr.Width "220"; HTMLAttr.Height "220"; ViewBox "0 0 220 220" ] [
                    circle [ Cx "110"; Cy "110"; R (string radius)
                              Fill "none"; Stroke "#2a2a3e"; StrokeWidth "8" ] []
                    circle [ Cx "110"; Cy "110"; R (string radius)
                              Fill "none"; Stroke color; StrokeWidth "8"
                              StrokeDasharray (string circumference)
                              StrokeDashoffset (string strokeDashoffset)
                              StrokeLinecap "round"
                              SVGAttr.Custom("style", "transform:rotate(-90deg);transform-origin:50% 50%;transition:stroke-dashoffset 1s linear") ] []
                ]
                div [ Class "timer-text" ] [
                    div [ Class "timer-display"; Style [ Color color ] ] [ str (formatTime model.SecondsLeft) ]
                    div [ Class "timer-phase" ] [ str (phaseLabel model.Phase) ]
                ]
            ]

            // Subject selector
            div [ Class "pomodoro-subject" ] [
                label [] [ str "Studying:" ]
                select [ Class "select"
                         OnChange (fun e ->
                            let v = (e.target :?> HTMLSelectElement).value
                            let sid = if v = "" then None else Some (int v)
                            dispatch (SetActiveSubject sid)) ] [
                    option [ Value "" ] [ str "— select subject —" ]
                    for sub in model.Subjects do
                        option [ Value (string sub.Id) ] [ str sub.Name ]
                ]
            ]

            // Controls
            div [ Class "pomodoro-controls" ] [
                button [ Class "btn-icon"; OnClick (fun _ -> dispatch ResetPomodoro) ] [ str "↺" ]
                match model.PomodoroStatus with
                | Running ->
                    button [ Class "btn-primary btn-large"; OnClick (fun _ -> dispatch PausePomodoro) ] [ str "⏸ Pause" ]
                | _ ->
                    button [ Class "btn-primary btn-large"; Style [ BackgroundColor color ]
                             OnClick (fun _ -> dispatch StartPomodoro) ] [ str "▶ Start" ]
                button [ Class "btn-icon"; OnClick (fun _ -> dispatch (SetPhase (match model.Phase with Work -> ShortBreak | _ -> Work))) ] [ str "⏭" ]
            ]

            // Session dots
            div [ Class "pomodoro-dots" ] [
                for i in 1..8 do
                    span [ Class (if i <= (model.PomodoroCount % 8) then "dot-filled" else "dot-empty") ] []
            ]
        ]
    ]

// ── Tasks Page ──

let tasksPage model dispatch =
    let pending = model.Tasks |> List.filter (fun t -> not t.Completed)
    let completed = model.Tasks |> List.filter (fun t -> t.Completed)

    div [ Class "page" ] [
        div [ Class "page-header" ] [
            div [] [
                h1 [] [ str "Tasks" ]
                p [ Class "subtitle" ] [ str (sprintf "%d pending · %d completed" pending.Length completed.Length) ]
            ]
            button [ Class "btn-primary"; OnClick (fun _ -> dispatch (OpenModal AddTaskModal)) ] [ str "+ New Task" ]
        ]

        div [ Class "tasks-layout" ] [
            // Pending
            div [ Class "card" ] [
                h2 [ Class "section-title" ] [ str "Pending" ]
                div [ Class "task-list-full" ] [
                    if pending.IsEmpty then
                        p [ Class "empty-state" ] [ str "No pending tasks — great work!" ]
                    for t in pending do
                        match findSubject model.Subjects t.SubjectId with
                        | None -> ()
                        | Some sub ->
                            div [ Class "task-item-full" ] [
                                button [ Class "task-check"; OnClick (fun _ -> dispatch (ToggleTask t.Id)) ] [ str "○" ]
                                div [ Class "task-info" ] [
                                    span [ Class "task-title-full" ] [ str t.Title ]
                                    subjectBadge sub
                                ]
                                button [ Class "btn-delete"; OnClick (fun _ -> dispatch (DeleteTask t.Id)) ] [ str "×" ]
                            ]
                ]
            ]

            // Completed
            div [ Class "card" ] [
                h2 [ Class "section-title" ] [ str "Completed" ]
                div [ Class "task-list-full" ] [
                    if completed.IsEmpty then
                        p [ Class "empty-state" ] [ str "No completed tasks yet." ]
                    for t in completed do
                        match findSubject model.Subjects t.SubjectId with
                        | None -> ()
                        | Some sub ->
                            div [ Class "task-item-full completed-task" ] [
                                button [ Class "task-check done"; OnClick (fun _ -> dispatch (ToggleTask t.Id)) ] [ str "✓" ]
                                div [ Class "task-info" ] [
                                    span [ Class "task-title-full strikethrough" ] [ str t.Title ]
                                    subjectBadge sub
                                ]
                                button [ Class "btn-delete"; OnClick (fun _ -> dispatch (DeleteTask t.Id)) ] [ str "×" ]
                            ]
                ]
            ]
        ]
    ]

// ── History Page ──

let historyPage model dispatch =
    let sessions = model.Sessions |> List.sortByDescending (fun s -> s.Date)
    let bySubject =
        model.Subjects
        |> List.map (fun sub ->
            let mins = model.Sessions |> List.filter (fun s -> s.SubjectId = sub.Id) |> List.sumBy (fun s -> s.Minutes)
            sub, mins)
        |> List.sortByDescending snd

    div [ Class "page" ] [
        div [ Class "page-header" ] [
            div [] [
                h1 [] [ str "Study History" ]
                p [ Class "subtitle" ] [ str (sprintf "%d sessions · %d min total" sessions.Length (totalMinutesAllTime model.Sessions)) ]
            ]
            button [ Class "btn-primary"; OnClick (fun _ -> dispatch (OpenModal LogSessionModal)) ] [ str "+ Log Session" ]
        ]

        div [ Class "history-layout" ] [
            // Subject breakdown
            div [ Class "card" ] [
                h2 [] [ str "Time by Subject" ]
                div [ Class "bar-chart" ] [
                    let maxMins = bySubject |> List.map snd |> List.tryHead |> Option.defaultValue 1
                    for (sub, mins) in bySubject do
                        let pct = if maxMins > 0 then mins * 100 / maxMins else 0
                        div [ Class "bar-row" ] [
                            span [ Class "bar-label" ] [ str sub.Name ]
                            div [ Class "bar-track" ] [
                                div [ Class "bar-fill"; Style [ Width (sprintf "%d%%" pct); BackgroundColor sub.Color ] ] []
                            ]
                            span [ Class "bar-value" ] [ str (sprintf "%d min" mins) ]
                        ]
                ]
            ]

            // Session log
            div [ Class "card" ] [
                h2 [] [ str "All Sessions" ]
                div [ Class "session-log" ] [
                    for s in sessions do
                        match findSubject model.Subjects s.SubjectId with
                        | None -> ()
                        | Some sub ->
                            div [ Class "log-item" ] [
                                div [ Class "log-date" ] [ str (s.Date.ToString("MMM d, yyyy")) ]
                                div [ Class "log-main" ] [
                                    span [ Class "log-dot"; Style [ BackgroundColor sub.Color ] ] []
                                    div [] [
                                        div [ Class "log-subject" ] [ str sub.Name ]
                                        if s.Notes <> "" then
                                            div [ Class "log-notes" ] [ str s.Notes ]
                                    ]
                                ]
                                div [ Class "log-mins" ] [ str (sprintf "%d min" s.Minutes) ]
                            ]
                ]
            ]
        ]
    ]

// ── Modals ──

let addSubjectModal model dispatch =
    div [ Class "modal-overlay"; OnClick (fun _ -> dispatch CloseModal) ] [
        div [ Class "modal"; OnClick (fun e -> e.stopPropagation()) ] [
            div [ Class "modal-header" ] [
                h2 [] [ str "Add Subject" ]
                button [ Class "modal-close"; OnClick (fun _ -> dispatch CloseModal) ] [ str "×" ]
            ]
            div [ Class "modal-body" ] [
                div [ Class "form-group" ] [
                    label [] [ str "Subject Name" ]
                    input [ Class "input"; Value model.NewSubjectName; Placeholder "e.g. Mathematics"
                            OnChange (fun e -> dispatch (UpdateNewSubjectName e.Value)) ]
                ]
                div [ Class "form-group" ] [
                    label [] [ str "Weekly Goal (minutes)" ]
                    input [ Class "input"; Type "number"; Value model.NewSubjectGoal
                            OnChange (fun e -> dispatch (UpdateNewSubjectGoal e.Value)) ]
                ]
                div [ Class "form-group" ] [
                    label [] [ str "Color" ]
                    div [ Class "color-grid" ] [
                        for c in subjectColors do
                            button [ Class (if model.NewSubjectColor = c then "color-btn selected" else "color-btn")
                                     Style [ BackgroundColor c ]
                                     OnClick (fun _ -> dispatch (UpdateNewSubjectColor c)) ] []
                    ]
                ]
            ]
            div [ Class "modal-footer" ] [
                button [ Class "btn-ghost"; OnClick (fun _ -> dispatch CloseModal) ] [ str "Cancel" ]
                button [ Class "btn-primary"; OnClick (fun _ -> dispatch SubmitNewSubject) ] [ str "Add Subject" ]
            ]
        ]
    ]

let addTaskModal model dispatch =
    div [ Class "modal-overlay"; OnClick (fun _ -> dispatch CloseModal) ] [
        div [ Class "modal"; OnClick (fun e -> e.stopPropagation()) ] [
            div [ Class "modal-header" ] [
                h2 [] [ str "Add Task" ]
                button [ Class "modal-close"; OnClick (fun _ -> dispatch CloseModal) ] [ str "×" ]
            ]
            div [ Class "modal-body" ] [
                div [ Class "form-group" ] [
                    label [] [ str "Task Title" ]
                    input [ Class "input"; Value model.NewTaskTitle; Placeholder "What do you need to do?"
                            OnChange (fun e -> dispatch (UpdateNewTaskTitle e.Value)) ]
                ]
                div [ Class "form-group" ] [
                    label [] [ str "Subject" ]
                    select [ Class "select"
                             OnChange (fun e ->
                                let v = (e.target :?> HTMLSelectElement).value
                                let sid = if v = "" then None else Some (int v)
                                dispatch (UpdateNewTaskSubject sid)) ] [
                        option [ Value "" ] [ str "— select —" ]
                        for sub in model.Subjects do
                            option [ Value (string sub.Id) ] [ str sub.Name ]
                    ]
                ]
            ]
            div [ Class "modal-footer" ] [
                button [ Class "btn-ghost"; OnClick (fun _ -> dispatch CloseModal) ] [ str "Cancel" ]
                button [ Class "btn-primary"; OnClick (fun _ -> dispatch SubmitNewTask) ] [ str "Add Task" ]
            ]
        ]
    ]

let logSessionModal model dispatch =
    div [ Class "modal-overlay"; OnClick (fun _ -> dispatch CloseModal) ] [
        div [ Class "modal"; OnClick (fun e -> e.stopPropagation()) ] [
            div [ Class "modal-header" ] [
                h2 [] [ str "Log Study Session" ]
                button [ Class "modal-close"; OnClick (fun _ -> dispatch CloseModal) ] [ str "×" ]
            ]
            div [ Class "modal-body" ] [
                div [ Class "form-group" ] [
                    label [] [ str "Subject" ]
                    select [ Class "select"
                             OnChange (fun e ->
                                let v = (e.target :?> HTMLSelectElement).value
                                let sid = if v = "" then None else Some (int v)
                                dispatch (UpdateLogSubjectId sid)) ] [
                        option [ Value "" ] [ str "— select —" ]
                        for sub in model.Subjects do
                            option [ Value (string sub.Id) ] [ str sub.Name ]
                    ]
                ]
                div [ Class "form-group" ] [
                    label [] [ str "Duration (minutes)" ]
                    input [ Class "input"; Type "number"; Value model.LogMinutes
                            OnChange (fun e -> dispatch (UpdateLogMinutes e.Value)) ]
                ]
                div [ Class "form-group" ] [
                    label [] [ str "Notes (optional)" ]
                    textarea [ Class "input"; Placeholder "What did you study?"
                               OnChange (fun e -> dispatch (UpdateLogNotes e.Value)) ] [ str model.LogNotes ]
                ]
            ]
            div [ Class "modal-footer" ] [
                button [ Class "btn-ghost"; OnClick (fun _ -> dispatch CloseModal) ] [ str "Cancel" ]
                button [ Class "btn-primary"; OnClick (fun _ -> dispatch SubmitLogSession) ] [ str "Log Session" ]
            ]
        ]
    ]

// ── Root View ──

let view model dispatch =
    div [ Class "app" ] [
        // Sidebar
        nav [ Class "sidebar" ] [
            div [ Class "logo" ] [
                span [ Class "logo-icon" ] [ str "◈" ]
                span [ Class "logo-text" ] [ str "StudyFlow" ]
            ]
            ul [ Class "nav-list" ] [
                navItem dispatch model.CurrentPage Dashboard "Dashboard" "⊞"
                navItem dispatch model.CurrentPage Pomodoro "Pomodoro" "◷"
                navItem dispatch model.CurrentPage Tasks "Tasks" "✓"
                navItem dispatch model.CurrentPage History "History" "◈"
            ]
            div [ Class "sidebar-footer" ] [
                div [ Class "sidebar-stat" ] [
                    span [] [ str (sprintf "%d sessions" model.Sessions.Length) ]
                ]
            ]
        ]

        // Main content
        main [ Class "main-content" ] [
            match model.CurrentPage with
            | Dashboard -> dashboardPage model dispatch
            | Pomodoro -> pomodoroPage model dispatch
            | Tasks -> tasksPage model dispatch
            | History -> historyPage model dispatch
        ]

        // Modals
        match model.Modal with
        | NoModal -> ()
        | AddSubjectModal -> addSubjectModal model dispatch
        | AddTaskModal -> addTaskModal model dispatch
        | LogSessionModal -> logSessionModal model dispatch
    ]

// ──────────────────────────────────────────────
// Subscriptions (timer tick)
// ──────────────────────────────────────────────

let timerSubscription model =
    let sub dispatch =
        let interval = 1000
        let handler _ = dispatch Tick
        let id = window.setInterval (handler, interval)
        { new System.IDisposable with member _.Dispose() = window.clearInterval id }
    [ ["timer"], sub ]

// ──────────────────────────────────────────────
// Entry Point
// ──────────────────────────────────────────────

Program.mkProgram init update view
|> Program.withSubscription timerSubscription
|> Program.withReactSynchronous "app-root"
|> Program.run
