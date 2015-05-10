SICSTUSHOME=/usr/local/opt/sicstus4.2.1
SICSTUSBIN= $(SICSTUSHOME)/bin
PL = $(SICSTUSBIN)/sicstus
SPLD = /usr/local/opt/sicstus4.2.1/bin/spld
SPLDFLAGS = --static --exechome=$(SICSTUSBIN)

ALL = verify

all: $(ALL)

%: %.sav
$(SPLD) $(SPLDFLAGS) $< -o $@ 

%.sav: %.pl
echo "compile('$<'). save_program('$@')." | $(PL)

clean:
rm -f $(ALL) *.sav
