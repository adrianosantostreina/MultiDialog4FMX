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
    BtnTestNull: TButton;
    BtnTest4: TButton;
    BtnTest3: TButton;
    BtnTest2: TButton;
    BtnTest1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    
    // Handlers para os botões de teste (Published)
    procedure OnTest1Click(Sender: TObject);
    procedure OnTest2Click(Sender: TObject);
    procedure OnTest3Click(Sender: TObject);
    procedure OnTest4Click(Sender: TObject);
    procedure OnTestNullClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoClickSim(Sender: TObject);
    procedure DoClickNao(Sender: TObject);
    procedure DoClickTalvez(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    //.SetMessage('Quer mesmo sair?')
    .SetMessage('Confirmar sair do sistema que agora tem uma tela de ' +
                'imagem maior e pode ser quebrada automaticamente quando ela abrir.' +
                'Isso é importante.')
    .Buttons
      .AddButton('Sim', DoClickSim, TAlphaColorRec.Green)  // ✅ Passando o método diretamente
      .AddButton('Não', DoClickNao, TAlphaColorRec.Brown)
      .AddButton('Cancelar', DoClickTalvez)
    .&End
    .Show;
end;

procedure TForm3.Button1Tap(Sender: TObject; const Point: TPointF);
begin
  //
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm3.OnTest1Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Aviso Simples')
    .SetMessage('Operação concluída com sucesso.')
    .SetCancelable(True)
    .Buttons
      .AddButton('OK') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;
end;

procedure TForm3.OnTest2Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Confirmação')
    .SetMessage('Deseja excluir este registro?')
    .Buttons
      .AddButton('Sim', DoClickSim, TAlphaColorRec.Red)
      .AddButton('Não', DoClickNao)
    .&End
    .Show;
end;

procedure TForm3.OnTest3Click(Sender: TObject);
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

procedure TForm3.OnTest4Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Avaliação')
    .SetMessage('Qual sua nota para o atendimento?')
    // Em Portrait < 600px, o botão "Pular" deve ficar full width abaixo
    .Buttons
      .AddButton('Ruim', TAlphaColorRec.Red)    // ✅ Nova sintaxe limpa
      .AddButton('Bom', TAlphaColorRec.Orange)  // ✅ Nova sintaxe limpa
      .AddButton('Ótimo', TAlphaColorRec.Green) // ✅ Nova sintaxe limpa
      .AddButton('Pular Avaliação', TAlphaColorRec.Lightgray) // ✅ Nova sintaxe limpa
    .&End
    .Show;
end;

procedure TForm3.OnTestNullClick(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Informação')
    .SetMessage('Esta mensagem tem botões sem evento OnClick (nil). Eles devem apenas fechar o diálogo.')
    .SetCancelable(True)
    .Buttons
      .AddButton('Entendi') // ✅ Nova sintaxe limpa
      .AddButton('Fechar')  // ✅ Nova sintaxe limpa
    .&End
    .Show;
end;

end.



