# MultiDialog4FMX

O **MultiDialog4FMX** é uma biblioteca leve e extensível para exibição de diálogos personalizados em aplicativos desenvolvidos com **Delphi FireMonkey (FMX)**.  
O objetivo é oferecer uma alternativa simples, fluente e multiplataforma para criar caixas de diálogo com título, mensagem e até **4 botões interativos**, mantendo consistência visual e facilidade de uso.

---

## ✨ **Principais recursos**

- **Builder Pattern fluente** – Criação de diálogos em um único encadeamento de comandos:
  ```delphi
  TMultiDialog4FMX.Dialog
    .SetTitle('Confirmação')
    .SetMessage('Tem certeza que deseja continuar?')
    .Buttons
      .AddButton('Sim', OnSimClick)
      .AddButton('Não', OnNaoClick)
      .End
    .Show;
  ```
- **Suporte multiplataforma** – Implementações específicas para Android e iOS, com possibilidade de extensão para Windows e outras plataformas FMX.
- **Até 4 botões configuráveis** – Evita poluir a interface com opções em excesso.
- **Compatível com eventos `OnClick` e `OnTap`**.
- **Ajuste automático da caixa de diálogo** – O tamanho se adapta dinamicamente à mensagem, com rolagem automática para textos muito grandes.
- **Interface limpa e reutilizável** – Código desacoplado por interfaces, com Factory e Util para facilitar o uso.

---

## 🚀 **Por que usar o MultiDialog4FMX?**

O Delphi FMX não possui, nativamente, uma solução simples para criar diálogos customizáveis de forma fluente e multiplataforma.  
O MultiDialog4FMX foi criado para preencher essa lacuna, permitindo criar diálogos profissionais, reutilizáveis e responsivos com poucas linhas de código, sem precisar lidar manualmente com layouts, botões e eventos para cada plataforma.

---

## 📌 **Roadmap**

- ✅ Implementação Android  
- 🚧 Implementação iOS  
- 🚧 Implementação Windows  
- 🔹 Suporte a temas e estilos personalizados  
- 🔹 Animações de entrada e saída da caixa de diálogo
- 🔹 Inclusão de ícone por tipo de mensagem

---

## 📜 Licença

Este projeto é distribuído sob a licença **MIT**, permitindo uso livre, modificação e contribuição para a comunidade Delphi.
