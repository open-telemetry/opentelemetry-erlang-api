defmodule OpenTelemetry.MixProject do
  use Mix.Project

  def project do
    [
      app: :open_telemetry,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      # We should never have dependencies
      deps: [],
      elixirc_paths: ["lib", "src"]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end
end
