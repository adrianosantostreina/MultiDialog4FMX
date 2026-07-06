unit Unit5;

interface

uses
  MultiDialog4FMX.Util,
  MultiDialog4FMX.Interfaces,

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
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnTest3: TButton;
    Button4: TButton;
    styGeralStyle: TStyleBook;
    Label1: TLabel;
    Button3: TButton;
    procedure BtnTest3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoClickSim(Sender: TObject);
    procedure DoClickNao(Sender: TObject);
    procedure DoClickTalvez(Sender: TObject);
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.DoClickSim(Sender: TObject);
begin
  Label1.Text := 'Clicou em Sim';
end;

procedure TForm5.DoClickNao(Sender: TObject);
begin
  Label1.Text := 'Clicou em Não';
end;

procedure TForm5.DoClickTalvez(Sender: TObject);
begin
  Label1.Text := 'Clicou em Talvez';
end;

procedure TForm5.BtnTest3Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Escolha uma opção')
    .SetMessage('Como deseja salvar o arquivo?')
    .Buttons
      .AddButton('Nuvem', DoClickSim, TAlphaColorRec.Blue)
      .AddButton('Local', DoClickNao, TAlphaColorRec.Green)
      .AddButton('Cancelar', DoClickTalvez)
    .&End
    .Show;
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(mdtWarning)
    .SetTitle('Aviso')
    .SetMessage('Sair do sistema?')
    .Buttons
      .AddButton('Yes',
      procedure ()
      begin
        Label1.Text := 'Yes';
      end
      )
      .AddButton('No',
      procedure ()
      begin
        Label1.Text := 'No';
      end
      )
    .&End
    .Show;
end;

procedure TForm5.Button2Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(mdtWarning)
    .SetTitle('Aviso')
    .SetMessage('Sair do sistema?')
    .Buttons
      .AddButton('Ok')
    .&End
    .Show;
end;

procedure TForm5.Button3Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Escolha uma opção')
    .SetMessage('Como deseja salvar o arquivo?')
    .Buttons
      .AddButton('Nuvem', DoClickSim, TAlphaColorRec.Blue)
      .AddButton('Local', DoClickNao, TAlphaColorRec.Green)
      .AddButton('Cancelar', DoClickTalvez)
      .AddButton('Teste Fim', DoClickTalvez)
    .&End
    .Show;
end;

procedure TForm5.Button4Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Aviso Simples')
    .SetMessage('Operação concluída com sucesso.')
    .SetCancelable(True)
    .Buttons
      .AddButton('OK', TAlphaColorRec.Null, 'BtnComumGreen') // ✅ Nova sintaxe limpa sem evento
      .AddButton('Cancelar', TAlphaColorRec.Null, 'BtnComumGray2') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;
end;

end.
