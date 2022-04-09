using ExprParsers
using Documenter

makedocs(;
    modules=[ExprParsers],
    authors="Stephan Sahm <stephan.sahm@gmx.de> and contributors",
    repo="https://github.com/jolin-io/ExprParsers.jl/blob/{commit}{path}#L{line}",
    sitename="ExprParsers.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://jolin-io.github.io/ExprParsers.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Manual" => "manual.md",
        "Library" => "library.md",
    ],
)

deploydocs(;
    repo="github.com/jolin-io/ExprParsers.jl",
    devbranch="main",
)
