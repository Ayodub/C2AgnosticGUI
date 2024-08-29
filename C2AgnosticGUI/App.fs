[<AutoOpen>]
module SysAdminManagementApp.App

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.Markup.Xaml
open Avalonia.Controls
open Avalonia.Styling
open Avalonia.Controls.Selection
open Avalonia.Controls.Shapes
open Avalonia.Controls.Primitives
open Avalonia.Controls.Templates

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()