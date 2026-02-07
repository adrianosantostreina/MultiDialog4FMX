# MultiDialog4FMX - Testes Unitários

Este diretório contém a suíte completa de testes unitários para a biblioteca **MultiDialog4FMX** usando o framework **DUnitX**.

---

## 📋 Estrutura de Testes

### Unidades de Teste

| Arquivo | Descrição | Casos de Teste |
|---------|-----------|----------------|
| **MultiDialog4FMX.Tests.Builder.pas** | Testa o padrão Builder e API fluente | 9 testes |
| **MultiDialog4FMX.Tests.Buttons.pas** | Testa funcionalidade de botões e validações | 11 testes |
| **MultiDialog4FMX.Tests.Factory.pas** | Testa factory de criação de diálogos | 3-5 testes (depende da plataforma) |
| **MultiDialog4FMX.Tests.Android.pas** | Testa lógica específica do Android | 8 testes |
| **MultiDialog4FMX.Tests.MemoryLeaks.pas** | Testa gerenciamento de memória | 5 testes |
| **MultiDialog4FMX.Tests.Mocks.pas** | Classes Mock para facilitar testes | Suporte |

### Cobertura de Testes

- ✅ **Builder Pattern**: SetTitle, SetMessage, SetCancelable, encadeamento fluente
- ✅ **Buttons**: 4 sobrecargas de AddButton, validação de limites (1-4 botões)
- ✅ **Factory**: Criação específica por plataforma
- ✅ **Android Logic**: Cálculo de altura, layout responsivo, resolução de form
- ✅ **Memory Management**: Detecção de vazamentos de memória

---

## 🚀 Como Executar os Testes

### Pré-requisitos

1. **DUnitX Framework**
   - Instale via GetIt Package Manager no RAD Studio
   - Ou clone de: https://github.com/VSoftTechnologies/DUnitX

2. **Compilador Delphi**
   - RAD Studio 10.3 Rio ou superior
   - FireMonkey (FMX) instalado

### Executar em Modo Console (Recomendado para CI/CD)

```bash
# 1. Navegar até o diretório de testes
cd "d:\2.2 GitHub Adriano Santos\MultiDialog4FMX\Tests"

# 2. Compilar o projeto (Win32)
msbuild MultiDialog4FMX.Tests.dproj /p:Config=Debug /p:Platform=Win32

# 3. Executar os testes
.\Win32\Debug\MultiDialog4FMX.Tests.exe --console

# 4. Verificar resultado (código de saída)
echo %ERRORLEVEL%
# 0 = todos os testes passaram
# >0 = algum teste falhou
```

### Executar em Modo GUI (Recomendado para Desenvolvimento)

```bash
# 1. Abrir o projeto no Delphi
# Arquivo: MultiDialog4FMX.Tests.dpr

# 2. Compilar e executar (F9)
# O GUI runner será exibido automaticamente

# 3. Clicar em "Run All" para executar todos os testes
```

### Executar Testes Específicos

No código, você pode usar atributos DUnitX:

```delphi
[Test]
[Ignore('Teste temporariamente desabilitado')]
procedure TestTemporarilyDisabled;
```

Ou via linha de comando:

```bash
# Executar apenas testes de uma fixture específica
MultiDialog4FMX.Tests.exe --fixture=TDialogBuilderTests
```

---

## 📊 Interpretando Resultados

### Console Output

```
[PASS] TDialogBuilderTests.TestSetTitle_StoresValue
[PASS] TDialogBuilderTests.TestSetMessage_StoresValue
[FAIL] TButtonTests.TestAddButton_ExceedsMaximum_RaisesException
  Expected exception 'Exception' was not raised
```

- **[PASS]**: Teste passou ✅
- **[FAIL]**: Teste falhou ❌
- **[IGNORED]**: Teste foi ignorado ⚠️

### XML Output (para CI/CD)

Os resultados são salvos em formato NUnit XML em:
```
Tests\Win32\Debug\dunitx-results.xml
```

Este arquivo pode ser integrado com sistemas de CI/CD como Jenkins, GitLab CI, etc.

---

## 🛠️ Adicionando Novos Testes

### 1. Criar Nova Fixture

```delphi
unit MultiDialog4FMX.Tests.NewFeature;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TNewFeatureTests = class
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestNewFeature;
  end;

implementation

procedure TNewFeatureTests.Setup;
begin
  // Inicialização antes de cada teste
end;

procedure TNewFeatureTests.TearDown;
begin
  // Limpeza após cada teste
end;

procedure TNewFeatureTests.TestNewFeature;
begin
  Assert.Pass('Teste implementado');
end;

initialization
  TDUnitX.RegisterTestFixture(TNewFeatureTests);

end.
```

### 2. Adicionar ao Projeto

Edite `MultiDialog4FMX.Tests.dpr` e adicione:

```delphi
uses
  // ... outras units ...
  MultiDialog4FMX.Tests.NewFeature in 'MultiDialog4FMX.Tests.NewFeature.pas';
```

### 3. Recompilar e Executar

```bash
msbuild MultiDialog4FMX.Tests.dproj /t:Rebuild
MultiDialog4FMX.Tests.exe --console
```

---

## 🐛 Debugging Testes

### No Delphi IDE

1. Abra `MultiDialog4FMX.Tests.dpr`
2. Coloque um breakpoint no teste desejado
3. Pressione **F9** para compilar e executar
4. O debugger irá parar no breakpoint

### Via Logs

Adicione mensagens de log nos testes:

```delphi
procedure TMyTests.TestSomething;
begin
  TDUnitX.CurrentRunner.Log('Debug: Valor = ' + IntToStr(MyValue));
  Assert.AreEqual(42, MyValue);
end;
```

---

## 📝 Convenções de Código

- **Nomenclatura de Testes**: `Test{MetodoOuFuncionalidade}_{Cenario}`
- **Assertions**: Use assertions descritivas com mensagens customizadas quando apropriado
- **Setup/TearDown**: Sempre limpe recursos em TearDown para evitar vazamentos
- **Isolamento**: Cada teste deve ser independente e não depender de outros

---

## 🔄 Integração Contínua (CI/CD)

### Exemplo para GitHub Actions

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Setup Delphi
        uses: embarcadero/setup-delphi@v1
        
      - name: Build Tests
        run: msbuild Tests\MultiDialog4FMX.Tests.dproj /p:Config=Release
        
      - name: Run Tests
        run: Tests\Win32\Release\MultiDialog4FMX.Tests.exe --console --xml=test-results.xml
        
      - name: Publish Results
        uses: EnricoMi/publish-unit-test-result-action@v1
        with:
          files: test-results.xml
```

---

## 📚 Recursos Adicionais

- **DUnitX GitHub**: https://github.com/VSoftTechnologies/DUnitX
- **DUnitX Wiki**: https://github.com/VSoftTechnologies/DUnitX/wiki
- **Delphi Testing Best Practices**: https://docwiki.embarcadero.com/RADStudio/en/Unit_Testing

---

## 🤝 Contribuindo

Ao adicionar novos recursos à biblioteca MultiDialog4FMX:

1. ✅ Escreva testes primeiro (TDD)
2. ✅ Garanta que todos os testes passem
3. ✅ Adicione documentação aos testes
4. ✅ Execute testes em múltiplas plataformas quando possível

---

## 📊 Status Atual

**Total de Testes**: ~36 testes  
**Cobertura Estimada**: ~80% da lógica de negócio  
**Plataformas Testadas**: Win32, Win64  
**Última Atualização**: Fevereiro 2026
