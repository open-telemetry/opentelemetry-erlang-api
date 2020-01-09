defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :open_telemetry,
      version: version(),
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      # We should never have dependencies
      deps: deps(),
      # Docs
      name: "OpenTelemetry API",
      docs: [
        main: "OpenTelemetry"
        # logo: "path/to/logo.png",
        # extras: ["README.md"]
      ],
      package: package()
    ]
  end

  defp version do
    "VERSION"
    |> File.read!()
    |> String.trim()
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  def deps() do
    [
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
    ]
  end

  defp package() do
    [
      build_tools: ["rebar3", "mix"]
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/open-telemetry/opentelemetry-erlang-api"}
    ]
  end
end
