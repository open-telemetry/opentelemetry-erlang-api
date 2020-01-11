defmodule OpenTelemetry.Tracer do
  @moduledoc """
  Defines a named tracer.

      defmodule MyApp.Tracer do
        use OpenTelemetry.Tracer,
          name: :my_app
      end

  Then used in your application:

      require MyApp.Tracer

      MyApp.Tracer.with_span "span-1" do
        ... do something ...
      end

  """

  @type t :: module

  @doc false
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      # @behaviour OpenTelemetry.Tracer

      @tracer_name opts[:name] || __MODULE__

      def get_tracer() do
        :opentelemetry.get_tracer(@tracer_name)
      end

      def start_span(name, opts \\ %{}) do
        :ot_tracer.start_span(get_tracer(), name, opts)
      end

      def end_span() do
        :ot_tracer.end_span(get_tracer())
      end

      def current_span_ctx() do
        :ot_tracer.current_span_ctx(get_tracer())
      end

      defdelegate set_attribute(key, value), to: :otel
      defdelegate set_attributes(attributes), to: :otel
      defdelegate add_event(name, attributes), to: :otel
      defdelegate add_events(events), to: :otel
      defdelegate add_links(links), to: :otel
      defdelegate set_status(status), to: :otel
      defdelegate update_name(name), to: :otel

      defmacro with_span(name, opts \\ quote(do: %{}), do: block) do
        quote do
          tracer = unquote(__MODULE__).get_tracer()
          :ot_tracer.start_span(tracer, unquote(name), unquote(opts))
          try do
            unquote(block)
          after
            :ot_tracer.end_span(tracer)
          end
        end
      end
    end
  end
end
