GHC = ghc
GHCFLAGS = -package Phases

default: TestSets.diff0 TestPhaseSet.diff0


TestSets: TestSets.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

TestPhaseSet: TestPhaseSet.hs
	$(GHC) $(GHCFLAGS) $^ -o $@

clean:
	rm -rf *.out *.run *.o *.hi Makefile.bak TestPhaseSet TestSets

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

