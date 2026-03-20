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
  TDialogAnimation = (danNone, danFade, danScale, danSlide);
  TDialogResultProc = reference to procedure(const AResult: TModalResult);

  IDialogButtonsBuilder = interface;
  /// <summary>
  /// Interface para construção fluente de diálogos.
  /// </summary>
  IDialogBuilder = interface
    ['{B7E3A1C5-4F82-4D9E-A3B1-0C8E2F7D5A61}']

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
    /// Define a animação de entrada/saída.
    /// </summary>
    function SetAnimation(const AAnimation: TDialogAnimation): IDialogBuilder;

    /// <summary>
    /// Define um SVG customizado para o ícone do diálogo.
    /// </summary>
    function SetIcon(const ASVG: string): IDialogBuilder;

    /// <summary>
    /// Define a cor de preenchimento do ícone (custom ou tipo).
    /// </summary>
    function SetIconColor(const AColor: TAlphaColor): IDialogBuilder;

    /// <summary>
    /// Define callback chamado ao fechar o diálogo com o resultado modal.
    /// </summary>
    function SetOnResult(const ACallback: TDialogResultProc): IDialogBuilder;

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
    ['{C2D4F8A7-1E93-4B5C-8D2E-3A9F6B0C7E84}']

    /// <summary>
    /// Adiciona um botão com evento OnClick padrão.
    /// </summary>
    function AddButton(const AText: string; const AOnClick: TNotifyEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão com método anônimo (TProc).
    /// </summary>
    function AddButton(const AText: string; const AOnSimpleClick: TProc; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão SEM evento (apenas fecha o diálogo).
    /// </summary>
    function AddButton(const AText: string; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão com evento OnTap (Toque).
    /// </summary>
    function AddButton(const AText: string; const AOnTap: TTapEvent; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Adiciona um botão sem evento que fecha automaticamente após ATimeout segundos,
    /// exibindo contagem regressiva no texto.
    /// </summary>
    function AddButton(const AText: string; const ATimeout: Integer; const AColor: TAlphaColor = TAlphaColorRec.Null; const AStyleLookup: string = ''; const AModalResult: TModalResult = mrOk): IDialogButtonsBuilder; overload;

    /// <summary>
    /// Finaliza a adição de botões e retorna ao builder principal.
    /// </summary>
    function &End: IDialogBuilder;
  end;

implementation

end.
