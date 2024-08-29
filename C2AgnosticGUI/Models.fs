[<AutoOpen>]
module SysAdminManagementApp.Models

open System
open Avalonia
open Avalonia.FuncUI.DSL
open Avalonia.Media
open Avalonia.Controls.Shapes
open Avalonia.Media.Imaging
open Microsoft.FSharp.Reflection

type OsType =
| Linux
| MacOs
| Windows
| Ubuntu

type Device = {
    Id:        int
    Name:      string
    IP:        string
    AssetName: string
    OsName:    string
    Position:  Point
    OsType:    Option<OsType>
}

type Terminal = {
    Name:         string
    Id:           int
    History:      List<CommandTrace>
    HistoryIndex: int
    CurrentPath:  string
    Command:      string
    Text:         string
    IsRenaming:   bool
    RenamingText: string
} with
    static member Default id = {
        Name         = $"Console {id}"
        Id           = id
        History      = []
        HistoryIndex = -1
        CurrentPath  =
            if Environment.OSVersion.Platform = PlatformID.Win32NT then
                Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            else
                $"{Environment.UserName}@{Environment.MachineName}:~"
        Command      = String.Empty
        Text         = String.Empty
        IsRenaming   = false
        RenamingText = $"Console {id}"
    }

and CommandTrace = {
    Id:      int
    Command: string
    Path:    string
    Output:  string
}

type Msg =
| AddTerminal
| SwitchView          of DeviceView
| OnDragMsg           of DragMsg
| OnConnectionMsg     of ConnectionMsg
| PointerMoved        of Point
| OnSetDeviceMsg      of SetDeviceMsg
| OnTerminalMsg       of TerminalId: int * TerminalMsg
| OnZoomMsg           of ZoomMsg

and DeviceView =
| ListView
| GraphView

and SetDeviceInfoMsg =
| SetOsName       of string
| SetAssetName    of string
| SetDeviceName   of string
| SetDeviceIP     of string
| SetDeviceOsType of OsType
| SetDeviceOs     of DeviceId: int * OsType

and DragMsg =
| DragStart of Device * StartPoint: Point
| Dragging  of Point
| DragEnd

and ConnectionMsg =
| SetConnection of DeviceId: int
| Disconnect    of Device1Id: int * Device2Id: int

and SetDeviceMsg =
| SetDevice
| ToggleSetDeviceForm   of MaybeExistingDevice: Option<Device>
| ToggleSetDeviceOsForm of DeviceId: int
| ToggleDeviceOptions   of DeviceId: int
| OnSetDeviceInfoMsg    of SetDeviceInfoMsg

and TerminalMsg =
| CommandChanged of string
| RunCommand
| CommandOutput  of CommandId: int * string
| PreviousCommand
| NextCommand
| SetCurrentPath of string
| SelectTerminal
| RemoveTerminal
| OnTerminalRenameMsg of TerminalRenameMsg

and TerminalRenameMsg =
| StartRenaming
| NameTextChanged of newName: string
| Rename

and ZoomMsg =
| ZoomIn
| ZoomOut

type StateModel = {
    // Terminal
    Terminals:          List<Terminal>
    SelectedTerminalId: int

    // Dvices
    Devices:  List<Device>
    View:     DeviceView

    // Set Device
    DeviceAcc:            Option<Device>
    SetDeviceFormVisible: bool
    SettingOsForDeviceId: Option<int>
    VisibleOptionsForDevicId:  Option<int>

    // dragging device
    DraggingDevice: Option<Device>
    DragStart:      Point
    DragOffset:     Point

    // Connection
    Connections:             Set<int * int>
    ConnectionStartDeviceId: Option<int>
    PointerPositionOnCanvas: Point

    ZoomLevel: float
}

let closeIconPath =
    Path.create [
        Path.data (Geometry.Parse ("M16,0 L15,0.01 L8,7 L1,0.01 L0,0 L0,1 L7,8 L0,15 L0,16 L1,16 L8,9 L15,16 L16,16 L16,15 L9,8 L16,1 L16,0 z"))
        Path.fill Brushes.Black
    ]

let plusIconPath =
    Path.create [
        Path.data (Geometry.Parse ("m5 0v4h4v1h-4v4h-1v-4h-4v-1h4v-4h1z"))
        Path.fill Brushes.Black
    ]

let helpIconPath =
    Path.create [
        Path.data (Geometry.Parse ("M12,2C6.477,2,2,6.477,2,12s4.477,10,10,10s10-4.477,10-10S17.523,2,12,2z M12,19c-0.553,0-1-0.447-1-1s0.447-1,1-1 s1,0.447,1,1S12.553,19,12,19z M14,14h-4v-2c0-1.101,0.899-2,2-2c0.553,0,1-0.447,1-1s-0.447-1-1-1s-1,0.447-1,1H9c0-1.654,1.346-3,3-3 s3,1.346,3,3S14.654,14,14,14z"))
        Path.fill Brushes.Gray
    ]

let zoomInIcon =
    Path.create [
        Path.data (Geometry.Parse ("M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm5 11h-4v4h-2v-4H7v-2h4V7h2v4h4v2zm-5-9C7.48 4 4 7.48 4 12s3.48 8 8 8 8-3.48 8-8-3.48-8-8-8z"))
        Path.fill Brushes.Black
    ]

let zoomOutIcon =
    Path.create [
        Path.data (Geometry.Parse ("M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm5 11H7v-2h10v2zm-5-9C7.48 4 4 7.48 4 12s3.48 8 8 8 8-3.48 8-8-3.48-8-8-8z"))
        Path.fill Brushes.Black
    ]

let desktopBitmap = new Bitmap("./Assets/desktop.png")
let private linuxBitmap = new Bitmap("./Assets/linux.png")
let private macOsBitmap = new Bitmap("./Assets/macos.png")
let private windowsBitmap = new Bitmap("./Assets/windows.png")
let private ubuntuBitmap = new Bitmap("./Assets/ubuntu.png")

type OsType with
    member this.Logo =
        match this with
        | Linux   -> linuxBitmap
        | MacOs   -> macOsBitmap
        | Windows -> windowsBitmap
        | Ubuntu  -> ubuntuBitmap

    static member GetLogo osType =
        match osType with
        | Linux   -> linuxBitmap
        | MacOs   -> macOsBitmap
        | Windows -> windowsBitmap
        | Ubuntu  -> ubuntuBitmap

    static member GeAllCases() =
        FSharpType.GetUnionCases(typeof<OsType>)
        |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> OsType)
        |> Array.toList