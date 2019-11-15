defmodule OpenTelemetry.Tracer do
  @moduledoc false

  defdelegate start_span(tracer, span_name, options), to: :ot_tracer
  defdelegate with_span(tracer, span_ctx), to: :ot_tracer
  defdelegate with_span(tracer, span_ctx, fun), to: :ot_tracer
  defdelegate current_span_ctx(tracer), to: :ot_tracer
  defdelegate get_binary_format(tracer), to: :ot_tracer
  defdelegate get_http_text_format(tracer), to: :ot_tracer
end
