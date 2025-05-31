CPU=$(shell "fpc" -iTP)
OS=$(shell "fpc" -iTO)
MYFPC=fpc
ifeq ($(OS),linux)
MYFPC=fpc4aros.sh
CPU=m68k
OS=amiga
endif
UNITSDIR=units/$(CPU)-$(OS)

AmiTranslate: $(UNITSDIR) AmiTranslate.lpr
	@echo Create units in $<
	@$(MYFPC) -g- -FU$< AmiTranslate.lpr
	@echo Units created in $<

$(UNITSDIR):
	@echo Create units dir $@
	@makedir $@

locale: ctfile catalog source

ctfile:
	@flexcat locale/AmiTranslate.cd locale/deutsch.ct NEWCTFILE locale/deutsch.ct

catalog:
	@flexcat locale/AmiTranslate.cd locale/deutsch.ct CATALOG Catalogs/deutsch/AmiTranslate.catalog
source:
	@flexcat locale/AmiTranslate.cd AmiTranslatelocale.pas=locale/FPCUnit.sd

all: ctfile locale AmiTranslate
	@echo done.

clean:
	@delete $(UNITSDIR)/#?.ppu
	@delete $(UNITSDIR)/#?.o  
