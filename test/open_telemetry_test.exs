defmodule OpenTelemetryTest.Tracer do
  use OpenTelemetry.Tracer,
    name: :my_tracer
end

defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  require OpenTelemetryTest.Tracer

  test "current_span tracks nesting" do
    _ctx1 = OpenTelemetryTest.Tracer.start_span("span-1")
    ctx2 = OpenTelemetryTest.Tracer.start_span("span-2")

    assert ctx2 == OpenTelemetryTest.Tracer.current_span_ctx()
  end

  test "closing a span makes the parent current" do
    ctx1 = OpenTelemetryTest.Tracer.start_span("span-1")
    ctx2 = OpenTelemetryTest.Tracer.start_span("span-2")

    assert ctx2 == OpenTelemetryTest.Tracer.current_span_ctx()
    OpenTelemetryTest.Tracer.end_span()
    assert ctx1 == OpenTelemetryTest.Tracer.current_span_ctx()
  end

  test "macro start_span" do
    alias OpenTelemetryTest.Tracer, as: Tracer

    Tracer.with_span "span-1" do
      Tracer.with_span "span-2" do
        Tracer.set_attribute("attr-1", "value-1")

        event1 = OpenTelemetry.event("event-1", [])
        event2 = OpenTelemetry.event("event-2", [])

        Tracer.add_events([event1, event2])
      end
    end
  end

end
