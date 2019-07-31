CXP_V_DIR=verilog-cxp

APPNAME=$(shell basename $(shell pwd))
BIGIP_SCRIPT=bigIP.tcl
timestamp := $(shell /bin/date "+%Y-%m-%d---%H-%M-%S")
ifndef CLOCK_FREQ_MHZ
export CLOCK_FREQ_MHZ=125
$(info set $$CLOCK_FREQ_MHZ to [${CLOCK_FREQ_MHZ}])
endif

all: chisel hw sw 

.PHONY: all sw chisel hw

help:
	@echo "------- INFO -------"
	@echo "export KEEP_HIERARCHY=1 # add keep_hierarchy annotation to all verilog modules"
	@echo "------- SUPPORTED MAKE TARGETS -------"
	@echo "make           : CXP SW + HW build"
	@echo "make hw        : Build Chisel for CXP"
	@echo "make sw        : Build software for CXP"
	@echo "make hw-clean  : Delete all generated hw files"
	@echo "make sw-clean  : Delete all generated sw files"
	@echo "make clean     : Delete all compiled code"
	@echo "------- END HELP -------"

sw:
	cp scripts/cxp.mk cpp/Makefile
	make -C cpp -j8
	tar -czf $(APPNAME).tar.gz -C ${CXP_V_DIR} accel.bit.bin parClockFreq.sh -C ../cpp Top -C ../cxp.sw-resources/utils set_perms setClocks.sh run.sh

chisel: 
	echo "$$(date +%s)" > start.log
	sbt "runMain top.Instantiator --verilog --testArgs cxp"

hw:
	@[ "${CXP_EXAMPLE}" ] || ( echo ">> CXP_EXAMPLE is not set"; exit 1 )
	cat cxp.hw-resources/SRAMVerilogAWS.v >> ${CXP_V_DIR}/Top.v
	if [ "${KEEP_HIERARCHY}" = "1" ]; then sed -i "s/^module/(* keep_hierarchy = \"yes\" *) module/g" ${CXP_V_DIR}/Top.v; fi
	cp cxp.hw-resources/build/* ${CXP_V_DIR}
	cp -r ${CXP_EXAMPLE}/02_coaxlink ${CXP_V_DIR}
	cp ${CXP_EXAMPLE}/03_scripts/*.tcl ${CXP_V_DIR}
	sed -i "s/\.\.\///g" ${CXP_V_DIR}/create_vivado_project.tcl
	cp -r ${CXP_EXAMPLE}/04_ref_design ${CXP_V_DIR}
	make -C ${CXP_V_DIR}
	echo "$$(date +%s)" > end.log

hw-clean:
	make -C ${CXP_V_DIR} clean
	rm -rf ${CXP_V_DIR}
	rm -f $(APPNAME).tar.gz ${BIGIP_SCRIPT}
	rm -rf target

sw-clean:
	make -C cpp clean
	rm -f $(APPNAME).tar.gz ${BIGIP_SCRIPT}

clean: hw-clean sw-clean

null: # Null target for regression testing purposes