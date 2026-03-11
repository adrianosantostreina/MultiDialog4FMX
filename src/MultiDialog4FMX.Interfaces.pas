unit MultiDialog4FMX.Interfaces;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,

  FMX.Types,
  FMX.Forms;

type
  TMultiDialogType = (mdtCustom, mdtWarning, mdtError, mdtInformation, mdtConfirmation, mdtQuestion);

  IDialogButtonsBuilder = interface;
  /// <summary>
  /// Interface para construção fluente de diálogos.
  /// </summary>
  IDialogBuilder = interface
    ['{D4F82B1C-3E97-4A5D-B2C0-8F1E5D9A6703}']

    /// <summary>
    /// Define o tipo do diálogo (ícone padrão).
    /// </summary>
    function SetType(AType: TMultiDialogType): IDialogBuilder;

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
    /// Define o tamanho de fonte para a mensagem e botões.
    /// </summary>
    function SetFontSize(const ASize: Single): IDialogBuilder;

    /// <summary>
    /// Define o raio de arredondamento das bordas do diálogo.
    /// </summary>
    function SetBorderRadius(const ARadius: Single): IDialogBuilder;

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
    ['{A6ADB133-2020-47C5-A4A8-7A11EADED3DC}']
    
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



