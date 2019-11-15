defmodule OpenTelemetry.Span do
  @moduledoc false

  defdelegate start_span(tracer, span_name, options), to: :ot_span
  defdelegate end_span(tracer, span_ctx), to: :ot_span
  defdelegate get_ctx(tracer, span), to: :ot_span
  defdelegate is_recording_events(tracer, span_ctx), to: :ot_span
  defdelegate set_attribute(tracer, span_ctx, key, value), to: :ot_span
  defdelegate set_attributes(tracer, span_ctx, attributes), to: :ot_span
  defdelegate add_events(tracer, span_ctx, events), to: :ot_span
  defdelegate set_status(tracer, span_ctx, status), to: :ot_span
  defdelegate update_name(tracer, span_ctx, name), to: :ot_span
 end
