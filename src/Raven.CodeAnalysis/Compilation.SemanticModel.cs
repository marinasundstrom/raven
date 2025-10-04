using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new();

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();

        EnsureSemanticModelsCreated();

        if (_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            return semanticModel;
        }

        if (!_syntaxTrees.Contains(syntaxTree))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }

        semanticModel = new SemanticModel(this, syntaxTree);
        _semanticModels[syntaxTree] = semanticModel;
        return semanticModel;
    }

    private void EnsureSemanticModelsCreated()
    {
        if (_sourceTypesInitialized || _isPopulatingSourceTypes)
            return;

        try
        {
            _isPopulatingSourceTypes = true;

            foreach (var syntaxTree in _syntaxTrees)
            {
                if (_semanticModels.ContainsKey(syntaxTree))
                    continue;

                var model = new SemanticModel(this, syntaxTree);
                _semanticModels[syntaxTree] = model;
            }

            _sourceTypesInitialized = true;
        }
        finally
        {
            _isPopulatingSourceTypes = false;
        }
    }
}
