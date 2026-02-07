# MultiDialog4FMX

**MultiDialog4FMX** é uma biblioteca leve, fluente e extensível para criação de diálogos personalizados em aplicativos **Delphi FireMonkey (FMX)**.
Ela oferece uma API simples e encadeada para exibir diálogos com título, mensagem e até **4 botões interativos**, mantendo a consistência visual e facilidade de uso entre plataformas.

---

## ✨ Principais Recursos

- **Padrão Builder Fluente** – Criação de diálogos em uma única cadeia de comandos.
- **Multiplataforma** – Otimizado para Android, com suporte adaptável para iOS e Windows.
- **Layout Responsivo** – Ajusta-se automaticamente para Portrait/Landscape e diferentes tamanhos de tela (ex: layout de botões 3+1 em smartphones).
- **API Limpa** – Suporte para **Métodos Anônimos**, **StyleLookup** e diálogos **Cancelable**.
- **Até 4 Botões** – Configuração flexível com eventos `OnClick` ou `OnTap`.
- **Personalização Visual** – Integração total com o `TStyleBook` do FMX.

---

## 🚀 Exemplos de Uso

### 1. Alerta Básico
Um diálogo simples com um único botão "OK". O botão fecha o diálogo automaticamente.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Sucesso')
  .SetMessage('Operação concluída com êxito.')
  .SetCancelable(True) // Permite fechar clicando no fundo
  .Buttons
    .AddButton('OK')
  .&End
  .Show;
```

### 2. Diálogo de Confirmação
Padrão Sim/Não com manipuladores de eventos.

```delphi
procedure TFormMain.OnConfirmClick(Sender: TObject);
begin
  // Executar ação de Sim
end;

// Uso
TMultiDialog4FMX.Dialog
  .SetTitle('Excluir Item')
  .SetMessage('Tem certeza que deseja excluir este item específico?')
  .Buttons
    .AddButton('Sim', OnConfirmClick)
    .AddButton('Não') // Sem handler = Fecha o diálogo
  .&End
  .Show;
```

### 3. Usando Métodos Anônimos
Mantenha seu código limpo definindo a lógica inline.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Ação Rápida')
  .SetMessage('Escolha uma ação para executar:')
  .Buttons
    .AddButton('Executar', 
      procedure 
      begin
        Log('Ação executada via método anônimo!');
      end
    )
    .AddButton('Cancelar')
  .&End
  .Show;
```

### 4. Estilos Personalizados (FMX Styles)
Aplique estilos FMX específicos aos botões usando `StyleLookup`.

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Aviso Visual')
  .SetMessage('Esta ação usa um estilo de perigo personalizado.')
  .Buttons
    .AddButton('Excluir', TAlphaColorRec.Null, 'dangerbutton_style') // Nome do estilo no StyleBook
    .AddButton('Cancelar', TAlphaColorRec.Null, 'transparent_style')
  .&End
  .Show;
```

### 5. Layout Responsivo (3+1)
Em smartphones no modo retrato, adicionar 4 botões aciona automaticamente um layout responsivo (3 botões no topo, 1 botão de largura total abaixo).

```delphi
TMultiDialog4FMX.Dialog
  .SetTitle('Avalie-nos')
  .SetMessage('Como foi sua experiência?')
  .Buttons
    .AddButton('Ruim', TAlphaColorRec.Red)
    .AddButton('Bom', TAlphaColorRec.Orange)
    .AddButton('Ótimo', TAlphaColorRec.Green)
    .AddButton('Pular Avaliação', TAlphaColorRec.LightGray) // Aparecerá com largura total abaixo
  .&End
  .Show;
```

### 6. Ícones e Tipos de Diálogo
Use `.SetType` para exibir ícones padrão. O diálogo ajusta automaticamente o layout para mostrar o ícone ao lado da mensagem.

| Tipo | Ícone Computado | Uso |
|------|-----------------|-----|
| `mdtWarning` | ⚠️ Alerta | Alertas críticos ou ações irreversíveis. |
| `mdtError` | ❌ Erro | Falhas do sistema ou problemas bloqueantes. |
| `mdtInformation` | ℹ️ Info | Informações gerais. |
| `mdtQuestion` | ❓ Pergunta | Confirmações do usuário. |
| `mdtConfirmation` | ✅ Sucesso | Operações bem-sucedidas. |

```delphi
TMultiDialog4FMX.Dialog
  .SetType(TMultiDialogType.mdtWarning) // Define o ícone de alerta
  .SetTitle('Sair')
  .SetMessage('Deseja realmente sair sem salvar?')
  .Buttons.AddButton('Sim').AddButton('Não').&End
  .Show;
```

### 7. Conteúdo com Rolagem
Se o texto da mensagem for muito grande para caber na tela (excedendo 90% da altura do dispositivo), o diálogo habilita automaticamente uma **barra de rolagem vertical** para a área de conteúdo, mantendo o Título e os Botões fixos. Nenhuma configuração extra é necessária!

---

## 📌 Instalação

1. Adicione o Global search path para a pasta `src`.
2. Adicione `MultiDialog4FMX.Util` à cláusula `uses` no formulário onde deseja chamar o diálogo.

---

## 📜 Licença

Este projeto é distribuído sob a licença **MIT**.

---

[🇺🇸 Read this documentation in English](README.md)
