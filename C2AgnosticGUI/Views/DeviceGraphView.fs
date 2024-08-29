module SysAdminManagementApp.DeviceGraphView

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Input
open Avalonia
open Avalonia.Controls.Shapes
open Avalonia.Media

open SysAdminManagementApp.Models
open Avalonia.Layout
open Avalonia.Controls.Primitives
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media.Imaging

open Avalonia.Media.Immutable



let private onPointerPressed (device: Device) (dispatch: Msg -> unit) (e: PointerPressedEventArgs) =
    let position = e.GetPosition(null)

    if e.GetCurrentPoint(null).Properties.IsLeftButtonPressed && e.ClickCount = 1 then
        dispatch (OnDragMsg (DragStart (device, position)))
        e.Handled <- true
    else if
        e.GetCurrentPoint(null).Properties.IsLeftButtonPressed
        && e.ClickCount = 2
    then
        dispatch (OnConnectionMsg (SetConnection device.Id))
        e.Handled <- true
    else if
        e.GetCurrentPoint(null).Properties.IsRightButtonPressed
        && e.ClickCount = 1
    then
        dispatch (OnSetDeviceMsg (ToggleDeviceOptions device.Id))
        e.Handled <- true
    else
        e.Handled <- false

let private onPointerMoved (dispatch: Msg -> unit) (e: PointerEventArgs) =
    let position = e.GetPosition(null)
    if e.GetCurrentPoint(null).Properties.IsLeftButtonPressed then
        dispatch (OnDragMsg (Dragging position))

    dispatch (PointerMoved position)
    e.Handled <- true

let private onPointerReleased (dispatch: Msg -> unit) (e: PointerReleasedEventArgs) =
    dispatch (OnDragMsg DragEnd)
    e.Handled <- true

let calculateVectorLength (vector: Point) =
    sqrt (vector.X * vector.X + vector.Y * vector.Y)

let normalizeVector (vector: Point) =
    let length = calculateVectorLength(vector)
    if length = 0.0 then
        Point(0.0, 0.0)
    else
        Point(vector.X / length, vector.Y / length)

let createConnectionPath (startPoint: Point) (endPoint: Point) (zoomLevel: float) (event: PointerPressedEventArgs -> unit) =
    let arrowLength = 10.0 * zoomLevel
    let arrowWidth = 5.0 * zoomLevel

    // Calculate direction vector and its normalized version
    let direction = Point(endPoint.X - startPoint.X, endPoint.Y - startPoint.Y)
    let normalizedDirection = normalizeVector direction

    // Calculate the base point of the arrowhead
    let arrowBase = Point(endPoint.X - normalizedDirection.X * arrowLength, endPoint.Y - normalizedDirection.Y * arrowLength)

    // Calculate points for the arrowhead
    let perpendicular = Point(-normalizedDirection.Y * arrowWidth, normalizedDirection.X * arrowWidth)
    let arrowLeft = Point(arrowBase.X + perpendicular.X, arrowBase.Y + perpendicular.Y)
    let arrowRight = Point(arrowBase.X - perpendicular.X, arrowBase.Y - perpendicular.Y)

    // Path data including the line and arrowhead
    let pathData = [
        startPoint
        endPoint
        arrowLeft
        endPoint
        arrowRight
        endPoint
    ]

    let segments =
        pathData
        |> List.map
            (fun point ->
                let linesegment = LineSegment()
                linesegment.Point <- point
                linesegment.IsStroked <- true

                linesegment :> PathSegment
            )
        |> List.toArray

    let figure = PathFigure()
    figure.StartPoint <- startPoint
    figure.Segments.AddRange segments
    figure.IsFilled <- true

    let pathGeometry = PathGeometry()
    pathGeometry.Figures.Add figure



    Path.create [
        Path.data pathGeometry
        Path.stroke (ImmutableSolidColorBrush(Colors.DarkOliveGreen))
        Path.strokeThickness (2.0 * zoomLevel)
        Path.fill (ImmutableSolidColorBrush(Colors.DarkOliveGreen)) // Set to `Transparent` if you only want the outline
        Line.onPointerPressed event
        Line.zIndex -1

        Line.tip "Disconnect"
    ]

let view (state: StateModel) (dispatch: Msg -> unit) : Types.IView =
    Grid.create [
        Grid.children [
            ScrollViewer.create [
                ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Auto
                ScrollViewer.verticalScrollBarVisibility ScrollBarVisibility.Auto

                ScrollViewer.content (
                    Canvas.create [
                        // let get screen size
                        let size = (Application.Current.ApplicationLifetime :?> IClassicDesktopStyleApplicationLifetime).MainWindow.Screens.Primary.WorkingArea.Size

                        let maxY =
                            state.Devices
                            |> List.map (fun device -> device.Position.Y + (state.ZoomLevel * 100.0))
                            |> List.append [ 0.0 ]
                            |> List.max

                        let maxX =
                            state.Devices
                            |> List.map (fun device -> device.Position.X + (state.ZoomLevel * 100.0))
                            |> List.append [ 0.0 ]
                            |> List.max



                        Canvas.width (max (float size.Width * (max state.ZoomLevel 1.0)) maxX)
                        Canvas.height (max ((float size.Height / 2.0) * (max state.ZoomLevel 1.0)) maxY)

                        Canvas.background (SolidColorBrush(Colors.LightSeaGreen))

                        // Add devices and connections
                        Canvas.children [

                            yield!
                                state.Devices
                                |> List.map
                                    (fun device ->
                                        StackPanel.create [
                                            Canvas.left (device.Position.X * state.ZoomLevel)
                                            Canvas.top (device.Position.Y * state.ZoomLevel)
                                            StackPanel.children [
                                                StackPanel.create [
                                                    StackPanel.children [
                                                        if state.SettingOsForDeviceId |> Option.exists (fun id -> id = device.Id) then
                                                            DockPanel.create [
                                                                DockPanel.children [
                                                                    yield!
                                                                        OsType.GeAllCases()
                                                                        |> List.map
                                                                            (fun osType ->
                                                                                Image.create [
                                                                                    Image.source osType.Logo
                                                                                    Image.width (state.ZoomLevel * 20.0)
                                                                                    Image.height (state.ZoomLevel * 20.0)
                                                                                    Image.margin (Thickness 2.0)

                                                                                    Image.onPointerReleased (fun _ -> dispatch (OnSetDeviceMsg (OnSetDeviceInfoMsg (SetDeviceOs (device.Id, osType)))))
                                                                                ]
                                                                                :> Types.IView
                                                                            )

                                                                    Button.create [
                                                                        Button.content closeIconPath
                                                                        Button.tip "Close"
                                                                        Button.onClick (fun _ -> dispatch (OnSetDeviceMsg (ToggleSetDeviceOsForm device.Id)))
                                                                    ]
                                                                ]
                                                            ]


                                                        let deviceName =
                                                            [
                                                                device.Name
                                                                device.AssetName
                                                            ]
                                                            |> List.filter (fun x -> x <> "" && x <> null)
                                                            |> String.concat "@"

                                                        if deviceName <> "" && deviceName <> null then
                                                            TextBlock.create [
                                                                TextBlock.text deviceName
                                                                TextBlock.horizontalAlignment HorizontalAlignment.Center
                                                                TextBlock.fontSize (12.0 * state.ZoomLevel)
                                                                TextBlock.foreground (SolidColorBrush(Colors.Black))
                                                                TextBlock.fontWeight FontWeight.Bold
                                                            ]

                                                        DockPanel.create [
                                                            DockPanel.children [
                                                                Grid.create [
                                                                    Grid.children [
                                                                        Image.create [
                                                                            Image.margin 0.0
                                                                            Image.maxWidth (state.ZoomLevel * 100.0)
                                                                            Image.maxHeight (state.ZoomLevel * 75.0)
                                                                            Image.source (desktopBitmap)
                                                                        ]

                                                                        device.OsType
                                                                        |> Option.map
                                                                            (fun osType ->
                                                                                Image.create [
                                                                                    Image.verticalAlignment VerticalAlignment.Top
                                                                                    Image.horizontalAlignment HorizontalAlignment.Center
                                                                                    Image.source osType.Logo
                                                                                    Image.maxWidth (state.ZoomLevel * 100.0)
                                                                                    Image.maxHeight (state.ZoomLevel * 50.0)
                                                                                    Image.margin (Thickness 2.0)
                                                                                ]
                                                                                :> Types.IView
                                                                            )
                                                                        |> Option.defaultValue (StackPanel.create [])
                                                                    ]

                                                                    Grid.onPointerPressed (onPointerPressed device dispatch)
                                                                    Grid.onPointerMoved (onPointerMoved dispatch)
                                                                    Grid.onPointerReleased (onPointerReleased dispatch)

                                                                    Grid.tip "Double left click to set connection point \nDouble right click to edit"
                                                                ]

                                                                if state.VisibleOptionsForDevicId |> Option.exists (fun id -> id = device.Id) then
                                                                    StackPanel.create [
                                                                        StackPanel.children [
                                                                            Button.create [
                                                                                Button.content "Edit"
                                                                                Button.onClick (fun _ -> dispatch (OnSetDeviceMsg (ToggleSetDeviceForm (Some device))))
                                                                            ]
                                                                            Button.create [
                                                                                Button.content "Set Logo"
                                                                                Button.onClick (fun _ -> dispatch (OnSetDeviceMsg (ToggleSetDeviceOsForm device.Id)))
                                                                            ]
                                                                        ]
                                                                    ]
                                                            ]
                                                        ]
                                                    ]
                                                ]
                                            ]

                                            StackPanel.background (SolidColorBrush(Colors.Transparent))


                                        ]
                                        :> Types.IView
                                    )

                            let deviceMap =
                                state.Devices
                                |> List.map (fun device -> device.Id, device)
                                |> Map.ofList

                            yield!
                                state.Connections
                                |> Seq.toList
                                |> List.map
                                    (fun (deviceId1, deviceId2) ->
                                        let device1 = deviceMap.[deviceId1]
                                        let device2 = deviceMap.[deviceId2]

                                        let heightHalfSize = (state.ZoomLevel * 100.0) / 2.0
                                        let widthHalfSize = (state.ZoomLevel * 75.0) / 2.0

                                        // calculate angle between two devices
                                        let angle = atan2 (device2.Position.Y - device1.Position.Y) (device2.Position.X - device1.Position.X)

                                        // calculate offset for connection path based on angle, if angle between 45 and 135 or 225 and 315 degrees then offset x, otherwise offset y
                                        let device2XOffset, device2YOffset =
                                            if angle > -3.14 / 4.0 && angle < 3.14 / 4.0 then
                                                -widthHalfSize, 0.0
                                            else if angle > 3.14 / 4.0 && angle < 3.14 * 3.0 / 4.0 then
                                                0.0, -heightHalfSize
                                            else if angle < -3.14 / 4.0 && angle > -3.14 * 3.0 / 4.0 then
                                                0.0, heightHalfSize
                                            else
                                                widthHalfSize, 0.0


                                        let device1Pos = Point((state.ZoomLevel * device1.Position.X + widthHalfSize), (state.ZoomLevel * device1.Position.Y + heightHalfSize))
                                        let device2Pos = Point((state.ZoomLevel * device2.Position.X + widthHalfSize + device2XOffset), (state.ZoomLevel * device2.Position.Y + heightHalfSize + device2YOffset))

                                        createConnectionPath device1Pos device2Pos state.ZoomLevel (fun _ -> dispatch (OnConnectionMsg (Disconnect (deviceId1, deviceId2))))
                                        :> Types.IView
                                    )

                            match state.ConnectionStartDeviceId with
                            | Some deviceId ->
                                let device = deviceMap.[deviceId]
                                let position = state.PointerPositionOnCanvas

                                Line.create [
                                    Line.startPoint (Point((state.ZoomLevel * device.Position.X), (state.ZoomLevel * device.Position.Y)))
                                    Line.endPoint position
                                    Line.stroke (SolidColorBrush(Colors.Black))
                                    Line.strokeThickness 2.0
                                ]
                                :> Types.IView

                            | None ->
                                ()

                        ]

                        Canvas.onPointerMoved (onPointerMoved dispatch)
                    ]
                    :> Types.IView
                )

                ScrollViewer.onPointerWheelChanged (fun e ->
                    // on wheel up zoom in and on wheel down zoom out if ctrl key is pressed otherwise scroll

                    if e.KeyModifiers = KeyModifiers.Control then
                        if e.Delta.Y > 0.0 then
                            dispatch (OnZoomMsg ZoomIn)
                        else
                            dispatch (OnZoomMsg ZoomOut)
                        e.Handled <- true
                    else
                        e.Handled <- false
                )

                ScrollViewer.background (SolidColorBrush(Colors.Aqua))
            ]

            DockPanel.create [
                DockPanel.horizontalAlignment  HorizontalAlignment.Right
                DockPanel.verticalAlignment    VerticalAlignment.Bottom
                DockPanel.children [
                    Button.create [
                        Button.content zoomInIcon
                        Button.tip "Zoom in"
                        Button.onClick (fun _ -> dispatch (OnZoomMsg ZoomIn))
                    ]
                    Button.create [
                        Button.content zoomOutIcon
                        Button.tip "Zoom out"
                        Button.onClick (fun _ -> dispatch (OnZoomMsg ZoomOut))
                    ]
                    Button.create [
                        Button.content (
                            Viewbox.create [
                                Viewbox.verticalAlignment VerticalAlignment.Center
                                Viewbox.width 22.0
                                Viewbox.child plusIconPath
                            ]
                        )
                        Button.tip "Add Device"
                        Button.onClick (fun _ -> dispatch (OnSetDeviceMsg (ToggleSetDeviceForm None)))
                    ]
                    :> Types.IView
                ]
            ]
        ]
    ]