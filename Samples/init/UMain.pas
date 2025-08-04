unit UMain;

interface

uses
  MultiDialog4FMX.Util,

  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoClickSim(Sender: TObject);
    procedure DoClickNao(Sender: TObject);
    procedure DoClickTalvez(Sender: TObject);
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.DoClickSim(Sender: TObject);
begin
  Label1.Text := 'Clicou em Sim';
end;

procedure TForm3.DoClickNao(Sender: TObject);
begin
  Label1.Text := 'Clicou em Não';
end;

procedure TForm3.DoClickTalvez(Sender: TObject);
begin
  Label1.Text := 'Clicou em Talvez';
end;


procedure TForm3.Button1Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Confirmação')
    .SetMessage('Tem certeza que deseja imprimir a NFC-e nesse exato momento?')
    .Buttons
      .AddButton('Sim', DoClickSim)  // ✅ Passando o método diretamente
      .AddButton('Não', DoClickNao)
      .AddButton('Talvez', DoClickTalvez)
    .&End
    .Show;
end;

procedure TForm3.Button1Tap(Sender: TObject; const Point: TPointF);
begin
  //
end;

end.
