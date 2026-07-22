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
  System.Threading,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.Memo,
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
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button1Tap(Sender: TObject; const Point: TPointF);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);

    // Handlers para os botões de teste (Published)
    procedure OnTest1Click(Sender: TObject);
    procedure OnTest2Click(Sender: TObject);
    procedure OnTest3Click(Sender: TObject);
    procedure OnTest4Click(Sender: TObject);
    procedure OnTestNullClick(Sender: TObject);
  private
    { Private declarations }
    // --- Sprint 7 demo (criados em runtime no FormCreate) ---
    FBtnDemoClose: TButton;
    FBtnDemoAwait: TButton;
    FMemoLog: TMemo;
    procedure DemoCloseClick(Sender: TObject);
    procedure DemoAwaitClick(Sender: TObject);
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

procedure TForm3.Button10Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    //.SetFontSize(14)
    //.SetBorderRadius(20)
    .SetType(TMultiDialogType.mdtQuestion) // Define o ícone
    .SetTitle('Saída')
    .SetMessage('Confirma saída do sistema?')
    .SetCancelable(True)
    .SetAnimation(danSlide)
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

procedure TForm3.Button11Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    //.SetFontSize(14)
    //.SetBorderRadius(20)
    .SetType(TMultiDialogType.mdtQuestion) // Define o ícone
    .SetTitle('Saída')
    .SetMessage('Confirma saída do sistema?')
    .SetCancelable(True)
    .Buttons
      .AddButton('Ok', 5)
      .AddButton('Cancelar')
    .&End
    .Show;
end;

procedure TForm3.Button12Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtConfirmation) // Define o ícone
    .SetTitle('Gravação')
    .SetMessage('Vamos gravar tudo.')
    .SetCancelable(True)
    .Buttons
      .AddButton('Ok',
        procedure()
        begin
          Label1.Text := 'Graviou.';
        end
      ,TAlphaColorRec.Null, 'BtnComumGreen') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;

  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtConfirmation) // Define o ícone
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
    .SetTheme(dthLight)
    .Buttons
      .AddButton('OK', TAlphaColorRec.Null, 'BtnComumGreen') // ✅ Nova sintaxe limpa sem evento
      .AddButton('Cancelar', TAlphaColorRec.Null, 'BtnComumGray2') // ✅ Nova sintaxe limpa sem evento
    .&End
    .Show;
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtConfirmation) // Define o ícone
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

procedure TForm3.Button7Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtConfirmation) // Define o ícone
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

procedure TForm3.Button8Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    //.SetFontSize(14)
    //.SetBorderRadius(20)
    .SetType(TMultiDialogType.mdtQuestion) // Define o ícone
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

procedure TForm3.Button9Click(Sender: TObject);
begin
  TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtInformation) // Define o ícone
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

const
  // Nomes legiveis dos eventos de telemetria, indexados por TDialogEventKind.
  C_KindNames: array[TDialogEventKind] of string =
    ('Enqueued', 'Shown', 'ButtonClicked', 'Cancelled', 'TimedOut', 'Closed', 'Suppressed');

procedure TForm3.FormCreate(Sender: TObject);
begin
  // ===== Sprint 7 demo: close programatico + await + telemetria =====
  FBtnDemoClose := TButton.Create(Self);
  FBtnDemoClose.Parent := Self;
  FBtnDemoClose.Position.X := 8;
  FBtnDemoClose.Position.Y := 744;
  FBtnDemoClose.Size.Width := 335;
  FBtnDemoClose.Size.Height := 44;
  FBtnDemoClose.Size.PlatformDefault := False;
  FBtnDemoClose.Text := 'Sprint7: Close por handle (2s)';
  FBtnDemoClose.OnClick := DemoCloseClick;

  FBtnDemoAwait := TButton.Create(Self);
  FBtnDemoAwait.Parent := Self;
  FBtnDemoAwait.Position.X := 8;
  FBtnDemoAwait.Position.Y := 792;
  FBtnDemoAwait.Size.Width := 335;
  FBtnDemoAwait.Size.Height := 44;
  FBtnDemoAwait.Size.PlatformDefault := False;
  FBtnDemoAwait.Text := 'Sprint7: ShowAndWait (TTask)';
  FBtnDemoAwait.OnClick := DemoAwaitClick;

  FMemoLog := TMemo.Create(Self);
  FMemoLog.Parent := Self;
  FMemoLog.Position.X := 8;
  FMemoLog.Position.Y := 844;
  FMemoLog.Size.Width := 335;
  FMemoLog.Size.Height := 150;
  FMemoLog.Size.PlatformDefault := False;
  FMemoLog.ReadOnly := True;

  // Telemetria -> Memo (marshalado para a UI thread; eventos podem vir de worker threads).
  TMultiDialog4FMX.OnDialogEvent :=
    procedure(const AInfo: TDialogEventInfo)
    var
      LMsg: string;
    begin
      LMsg := Format('%s | "%s" | r=%d | %dms',
        [C_KindNames[AInfo.Kind], AInfo.Title, AInfo.Result, AInfo.ElapsedMs]);
      TThread.Queue(nil,
        procedure
        begin
          FMemoLog.Lines.Add(LMsg);
        end);
    end;
end;

procedure TForm3.DemoCloseClick(Sender: TObject);
var
  LHandle: IDialogHandle;
begin
  LHandle := TMultiDialog4FMX.Dialog
    .SetType(TMultiDialogType.mdtInformation)
    .SetTitle('Close program'#225'tico')
    .SetMessage('Este di'#225'logo fecha sozinho em 2 segundos via IDialogHandle.Close(mrCancel).')
    .Buttons
      .AddButton('Aguarde...')
    .&End
    .ShowGetHandle;

  // Agenda o fechamento por codigo apos 2s. LHandle (interface) e capturado pelo
  // closure -> mantido vivo ate o fechamento.
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(2000);
      TThread.Queue(nil,
        procedure
        begin
          if LHandle.IsActive then
            LHandle.Close(mrCancel);
        end);
    end).Start;
end;

procedure TForm3.DemoAwaitClick(Sender: TObject);
begin
  // ShowAndWait BLOQUEIA a thread chamadora ate o dialogo resolver — por isso roda
  // dentro de um TTask (worker thread), nunca na main thread (levantaria
  // EDialogAwaitOnMainThread).
  TTask.Run(
    procedure
    var
      LRes: TModalResult;
    begin
      LRes := TMultiDialog4FMX.Dialog
        .SetType(TMultiDialogType.mdtQuestion)
        .SetTitle('Await')
        .SetMessage('ShowAndWait rodando numa worker thread (TTask). Escolha uma op'#231#227'o.')
        .Buttons
          .AddButton('Sim', TAlphaColorRec.Null, '', mrYes)
          .AddButton('N'#227'o', TAlphaColorRec.Null, '', mrNo)
        .&End
        .ShowAndWait;
      TThread.Queue(nil,
        procedure
        begin
          Label1.Text := 'Await retornou: ' + IntToStr(LRes);
        end);
    end);
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
        TAlphaColorRec.Red, '')    // ✅ Teste Anônimo + Style
      .AddButton('Bom', TAlphaColorRec.Orange)  // ✅ Nova sintaxe limpa
      .AddButton('Ótimo', TAlphaColorRec.Green) // ✅ Nova sintaxe limpa
      .AddButton('Pular Avaliação', TAlphaColorRec.Brown) // ✅ Nova sintaxe limpa
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


{ =========================================================================
  Stub TSkSvg — evita EClassNotFound em ambientes sem Skia4Delphi.
  O StyleBook deste sample foi criado em um IDE com Skia instalado e contém
  um TSkSvg decorativo em um dos estilos de botão.  A biblioteca em si não
  usa TSkSvg — apenas TPath (FMX nativo).
  ========================================================================= }
type
  TSkSvgPlaceholder = class(TFmxObject);

initialization
  // Registra placeholder apenas se TSkSvg não estiver disponível (sem Skia4Delphi).
  // Quando Skia estiver instalado, GetClass('TSkSvg') retorna a classe real.
  if not Assigned(GetClass('TSkSvg')) then
    RegisterClassAlias(TSkSvgPlaceholder, 'TSkSvg');

end.



