GHC = ghc
#GHCFLAGS = -package time-phase -prof -auto-all -rtsopts
GHCFLAGS = -package time-phase

default: unit use
	@echo "Success!"

unit: \
 TestSets.diff0 \
 PointSetTest.diff0 \
 PhaseTest.diff0 \
 ItemTest.diff0 \

use: \
 test.phases.diff \
 duration.phases.diff \
 dateformats.phases.10y.diff \
 timeformats.phases.7d.diff \
 months.phases.diff \
 startend.phases.diff \
 from-until.phases.diff \
 to.phases.diff \
 nth.phases.diff \
 easter.phases.diff \
 of.phases.diff \
 now.phases.cur.out
	@echo "Success!"


TestSets: TestSets.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

PointSetTest: PointSetTest.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

PhaseTest: PhaseTest.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

ItemTest: ItemTest.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

clean:
	rm -rf *.out *.run *.o *.hi *.p_o *.p_hi *.prof Makefile.bak TestSets PointSetTest PhaseTest ItemTest

%.phases.cur.out: %.phases
	timeout 5s ../dist/build/phase-calendar/phase-calendar $< > $@ || echo "Failed with $$?" > $@

%.phases.out: %.phases
	timeout 5s ../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" $< > $@ || echo "Failed with $$?" > $@

%.phases.7d.out: %.phases
	timeout 5s ../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" --days 7 $< > $@ || echo "Failed with $$?" > $@

%.phases.10y.out: %.phases
	timeout 5s ../dist/build/phase-calendar/phase-calendar --start "2012-04-17 00:00:00" --days 3650 $< > $@ || echo "Failed with $$?" > $@

%.diff: %.ref %.out
	diff -u $^

%.diff0: %.out
	diff -u /dev/null $^

%.out: %
	timeout 5s ./$< > $@ || echo "Failed with $$?" > $@

%.run: %
	timeout 5s ./$<
	touch $@

%.hi: %.o
	@:

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.o: %.lhs
	$(GHC) $(GHCFLAGS) -c $< -o $@

FORCE:

.SECONDARY:

