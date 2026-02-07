unit UMain;

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
  TForm3 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    BtnTestNull: TButton;
    BtnTest4: TButton;
    BtnTest3: TButton;
    BtnTest2: TButton;
    BtnTest1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    styGeralStyle: TStyleBook;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);

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

procedure TForm3.Button2Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Erro')
    .SetMessage
    (
    'O sistema não conseguiu acessar o '+
    'banco de dados remoto que fica no endereço ' +
    'configurado no arquivo de configuração de config.ini. ' +
    'Esse arquivo pode ser encontrado no diretório do sistema.' +
    'Caso não encontre, entre em contato com o administrador.' +
    #13#10 +
    '-------------' +
    #13#10 +
    'O sistema não conseguiu acessar o '+
    'banco de dados remoto que fica no endereço ' +
    'configurado no arquivo de configuração de config.ini. ' +
    'Esse arquivo pode ser encontrado no diretório do sistema.' +
    'Caso não encontre, entre em contato com o administrador.'
    )
    // Em Portrait < 600px, o botão "Pular" deve ficar full width abaixo
    .Buttons
      .AddButton('Ruim', DoClickSim, TAlphaColorRec.Red)    // ✅ Nova sintaxe limpa
      .AddButton('Bom', TAlphaColorRec.Orange)  // ✅ Nova sintaxe limpa
      .AddButton('Ótimo', TAlphaColorRec.Green) // ✅ Nova sintaxe limpa
      .AddButton('Pular Avaliação', TAlphaColorRec.Lightgray) // ✅ Nova sintaxe limpa
    .&End
    .Show;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Erro')
    .SetMessage('Mensagem com método anônimo. ')
    // Em Portrait < 600px, o botão "Pular" deve ficar full width abaixo
    .Buttons
      .AddButton(
      'Ruim',
      procedure()
      begin
        Label1.Text := 'Funcionou';
      end
      , TAlphaColorRec.Red)    // ✅ Nova sintaxe limpa
      .AddButton('Bom', TAlphaColorRec.Orange)  // ✅ Nova sintaxe limpa
      .AddButton('Ótimo', TAlphaColorRec.Green) // ✅ Nova sintaxe limpa
      .AddButton('Pular Avaliação', TAlphaColorRec.Lightgray) // ✅ Nova sintaxe limpa
    .&End
    .Show;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Aviso Simples')
    .SetMessage('Operação concluída com sucesso.')
    .SetCancelable(True)
    .Buttons
      .AddButton('OK', TAlphaColorRec.Null, 'BtnComumGreen') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtWarning) // Define o ícone
    .SetTitle('Saída')
    .SetMessage('Confirma saída do sistema?')
    .SetCancelable(True)
    .Buttons
      .AddButton('Sim',
        procedure()
        begin
          Label1.Text := 'Clicou em SIM.';
        end
      ,TAlphaColorRec.Null, 'BtnComumGreen') // ✅ Nova sintaxe limpa sem evento
      .AddButton('Não', TAlphaColorRec.Null, 'BtnComumRed') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;
end;

procedure TForm3.Button6Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetTitle('Erro')
    .SetType(TMultiDialogType.mdtError)
    .SetMessage
    (
    'O sistema não conseguiu acessar o '+
    'banco de dados remoto que fica no endereço ' +
    'configurado no arquivo de configuração de config.ini. ' +
    'Esse arquivo pode ser encontrado no diretório do sistema.' +
    'Caso não encontre, entre em contato com o administrador.' +
    #13#10 +
    '-------------' +
    #13#10 +
    'O sistema não conseguiu acessar o '+
    'banco de dados remoto que fica no endereço ' +
    'configurado no arquivo de configuração de config.ini. ' +
    'Esse arquivo pode ser encontrado no diretório do sistema.' +
    'Caso não encontre, entre em contato com o administrador.'
    )
    // Em Portrait < 600px, o botão "Pular" deve ficar full width abaixo
    .Buttons
      .AddButton('Ruim', DoClickSim, TAlphaColorRec.Red)    // ✅ Nova sintaxe limpa
      .AddButton('Bom', TAlphaColorRec.Orange)  // ✅ Nova sintaxe limpa
      .AddButton('Ótimo', TAlphaColorRec.Green) // ✅ Nova sintaxe limpa
      .AddButton('Pular Avaliação', TAlphaColorRec.Lightgray) // ✅ Nova sintaxe limpa
    .&End
    .Show;
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
    .SetCancelable(True)
    .Buttons
      .AddButton('Sim', DoClickSim, TAlphaColorRec.Red)
      .AddButton('Não')
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
      .AddButton('Ruim', 
        procedure
        begin
          Label1.Text := 'Avaliação: Ruim (via Anônimo)';
        end,
        TAlphaColorRec.Red, 'trasparentstyle')    // ✅ Teste Anônimo + Style
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



