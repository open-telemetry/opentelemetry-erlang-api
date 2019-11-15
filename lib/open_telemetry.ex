defmodule OpenTelemetry do
  # Shortcuts that operate implicitly on the current Tracer
  defdelegate start_span(name), to: :otel
  defdelegate start_span(name, options), to: :otel
  defdelegate with_span(span_ctx), to: :otel
  defdelegate with_span(span_ctx, fun), to: :otel
  defdelegate current_span_ctx(), to: :otel
  defdelegate end_span(), to: :otel

  # Manage the Tracer
  defdelegate set_default_tracer(tracer), to: :opentelemetry
  defdelegate get_tracer(), to: :opentelemetry
  defdelegate get_tracer(name), to: :opentelemetry

  # Helpers to build OpenTelemetry structured types
  defdelegate timestamp(), to: :opentelemetry
  defdelegate links(link_list), to: :opentelemetry
  defdelegate link(trace_id, span_id, attributes, trace_state), to: :opentelemetry
  defdelegate event(name, attributes), to: :opentelemetry
  defdelegate timed_event(timestamp, name, attributes), to: :opentelemetry
  defdelegate timed_event(timestamp, event), to: :opentelemetry
  defdelegate timed_events(timed_event_list), to: :opentelemetry
  defdelegate status(code, message), to: :opentelemetry

  # Do these need to be in a public API?
  defdelegate generate_trace_id(), to: :opentelemetry
  defdelegate generate_span_id(), to: :opentelemetry
end
