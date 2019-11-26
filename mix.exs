defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :open_telemetry,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      # We should never have dependencies
      deps: deps(),
      # Docs
      name: "OpenTelemetry API",
      # source_url: "https://github.com/USER/PROJECT",
      # homepage_url: "http://YOUR_PROJECT_HOMEPAGE",
      docs: [
        main: "OpenTelemetry"
        # logo: "path/to/logo.png",
        # extras: ["README.md"]
      ]
    ]
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
end
