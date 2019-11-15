defmodule OpenTelemetryTest do
  use ExUnit.Case, async: true

  test "current_span tracks nesting" do
    _ctx1 = OpenTelemetry.start_span("span-1")
    ctx2 = OpenTelemetry.start_span("span-2")

    assert ctx2 == OpenTelemetry.current_span_ctx()
  end

  test "closing a span makes the parent current" do
    ctx1 = OpenTelemetry.start_span("span-1")
    ctx2 = OpenTelemetry.start_span("span-2")

    assert ctx2 == OpenTelemetry.current_span_ctx()
    OpenTelemetry.end_span()
    assert ctx1 == OpenTelemetry.current_span_ctx()
  end
end
