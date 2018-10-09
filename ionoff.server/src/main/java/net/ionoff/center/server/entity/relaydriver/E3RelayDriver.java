package net.ionoff.center.server.entity.relaydriver;

import net.ionoff.center.server.entity.RelayDriver;

public class E3RelayDriver extends RelayDriver {

    static {
        RelayDriver.MODELS.put(model(), E3RelayDriver.class);
    }

    private static String model() {
        return "IONOFF_E3";
    }

    @Override
    public boolean autoRevert() {
        return true;
    }

    @Override
    public int getInput() {
        return 3;
    }

    @Override
    public int getOutput() {
        return 3;
    }

    @Override
    public String getModel() {
        return model();
    }

    @Override
    public int getOnlineBuffer() {
        return 49000;
    }

    @Override
    public String getProtocol() {
        return "mqtt";
    }

    @Override
    public String getCommandStatus() {
        return "{ioget}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "1}";
    }

    @Override
    public String getCommandCloseRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "0}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
        int revert = 0;
        if (autoRevert != null && autoRevert.intValue() > 0) {
            revert = autoRevert.intValue();
        }
        return "{ioseto" + (relayIndex + 1) + "1"+ revert + "}";
    }

    @Override
    public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
        int revert = 0;
        if (autoRevert != null && autoRevert.intValue() > 0) {
            revert = autoRevert.intValue();
        }
        return "{ioseto" + (relayIndex + 1) + "0" + revert +"}";
    }

	@Override
	public int getOneSecondDelay() {
		return 650;
	}
}
