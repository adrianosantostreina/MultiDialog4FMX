unit MultiDialog4FMX.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,

  FMX.Types,
  FMX.Forms;

type
  IDialogButtonsBuilder = interface;
  /// <summary>
  /// Interface para construção fluente de diálogos.
  /// </summary>
  IDialogBuilder = interface
    ['{A1B2C3D4-E5F6-47A8-9B0C-ABCDEF123456}']
    
    /// <summary>
    /// Define o título do diálogo.
    /// </summary>
    function SetTitle(const ATitle: string): IDialogBuilder;
    
    /// <summary>
    /// Define a mensagem do diálogo.
    /// </summary>
    function SetMessage(const AMessage: string): IDialogBuilder;
    
    /// <summary>
    /// Define se o diálogo pode ser fechado clicando fora dele (no overlay).
    /// </summary>
    function SetCancelable(const Value: Boolean): IDialogBuilder;

    /// <summary>
    /// Acessa o construtor de botões.
    /// </summary>
    function Buttons: IDialogButtonsBuilder;
    /// <summary>
    /// Exibe o diálogo.
    /// </summary>
    function Show: IDialogBuilder; overload;
    
    /// <summary>
    /// Exibe o diálogo em um formulário específico.
    /// </summary>
    function Show(const AForm: TCommonCustomForm): IDialogBuilder; overload;
  end;

  /// <summary>
  /// Interface para adição fluente de botões.
  /// </summary>
  IDialogButtonsBuilder = interface
    ['{B1C2D3E4-F5A6-47A8-9B0C-ABCDEF654321}'] // Corrigido GUID duplicado se houver, mantendo o original por segurança
    
    /// <summary>
    /// Adiciona um botão com evento OnClick padrão.
    /// </summary>
    function AddButton(const AText: string; const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    
    /// <summary>
    /// Adiciona um botão com método anônimo (TProc).
    /// </summary>
    function AddButton(const AText: string; const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão SEM evento (apenas fecha o diálogo).
    /// </summary>
    function AddButton(const AText: string; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão com evento OnTap (Toque).
    /// </summary>
    function AddButton(const AText: string; const AOnTap: TTapEvent; const AColor: TAlphaColor  = TAlphaColorRec.Null; const AStyleLookup: string = ''): IDialogButtonsBuilder; overload;
    
    /// <summary>
    /// Finaliza a adição de botões e retorna ao builder principal.
    /// </summary>
    function &End: IDialogBuilder;
  end;

implementation

end.



