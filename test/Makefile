GHC = ghc
GHCFLAGS = -package Phases

default: \
 TestSets.diff0 \
 TestPhaseSet.diff0 \
 test.phases.diff \
 duration.phases.diff \
 dateformats.phases.10y.diff \
 timeformats.phases.7d.diff \
 months.phases.diff \
 startend.phases.diff \
 from-until.phases.diff \
 to.phases.diff \
 nth.phases.diff \
 easter.phases.diff
	@echo "Success!"


TestSets: TestSets.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

TestPhaseSet: TestPhaseSet.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

clean:
	rm -rf *.out *.run *.o *.hi Makefile.bak TestPhaseSet TestSets

%.phases.out: %.phases
	../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" $< > $@

%.phases.7d.out: %.phases
	../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" --days 7 $< > $@

%.phases.10y.out: %.phases
	../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" --days 3650 $< > $@

%.diff: %.ref %.out
	diff -u $^

%.diff0: %.out
	diff -u /dev/null $^

%.out: %
	./$< > $@

%.run: %
	./$<
	touch $@

%.hi: %.o
	@:

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.o: %.lhs
	$(GHC) $(GHCFLAGS) -c $< -o $@

FORCE:

.SECONDARY:

