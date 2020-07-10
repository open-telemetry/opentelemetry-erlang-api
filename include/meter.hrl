%% macros for meters
%% register a meter for an application with opentelemetry:register_application_meter(AppName)

-define(KIND_COUNTER, ot_counter).
-define(KIND_UPDOWN_COUNTER, ot_updown_counter).
-define(KIND_VALUE_RECORDER, ot_value_recorder).
-define(KIND_VALUE_OBSERVER, ot_value_observer).
-define(KIND_SUM_OBSERVER, ot_sum_observer).
-define(KIND_UPDOWN_SUM_OBSERVER, ot_updown_sum_observer).

-define(current_meter, opentelemetry:get_meter(?MODULE)).

-define(new_counter(Meter, Name, Opts),
        ot_counter:new(?current_meter, Name, Opts)).

-define(new_updown_counter(Meter, Name, Opts),
        ot_updown_counter:new(?current_meter, Name, Opts)).

-define(new_value_recorder(Meter, Name, Opts),
        ot_value_recorder:new(?current_meter, Name, Opts)).

-define(new_sum_observer(Meter, Name, Opts),
        ot_sum_observer:new(?current_meter, Name, Opts)).

-define(new_updown_observer(Meter, Name, Opts),
        ot_updown_observer:new(?current_meter, Name, Opts)).

-define(new_value_observer(Meter, Name, Opts),
        ot_value_observer:new(?current_meter, Name, Opts)).

-define(new_instruments(List),
        ot_meter:new_instruments(?current_meter, List)).

-define(counter_add(BoundCounter, Number),
        ot_counter:add(BoundCounter, Number)).

-define(counter_add(Name, Number, LabelSet),
        ot_counter:add(?current_meter, Name, Number, LabelSet)).

-define(measure_record(BoundMeasure, Number),
        ot_measure:record(BoundMeasure, Number)).

-define(measure_record(Name, Number, LabelSet),
        ot_measure:record(?current_meter, Name, Number, LabelSet)).

-define(bind(Name, LabelSet),
        ot_meter:bind(?current_meter, Name, LabelSet)).

-define(release(BoundInstrument),
        ot_meter:release(?current_meter, BoundInstrument)).

-define(record(Name, Number, LabelSet),
        ot_meter:record(?current_meter, Name, Number, LabelSet)).

-define(record(BoundInstrument, Number),
        ot_meter:record(BoundInstrument, Number)).

-define(record_batch(LabelSet, Measurements),
        ot_meter:record_batch(?current_meter, LabelSet, Measurements)).
