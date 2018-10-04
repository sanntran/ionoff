package net.ionoff.center.server.entity.relaydriver;

import net.ionoff.center.server.entity.RelayDriver;

public class P4RelayDriver extends RelayDriver {

    static {
        RelayDriver.MODELS.put(model(), P4RelayDriver.class.getClass());
    }

    private static String model() {
        return "IONOFF_P4";
    }

    @Override
    public int getInput() {
        return 4;
    }

    @Override
    public int getOutput() {
        return 4;
    }

    @Override
    public String getModel() {
        return model();
    }

    @Override
    public int getOnlineBuffer() {
        return 32000;
    }

    @Override
    public String getProtocol() {
        return "tcp";
    }

    @Override
    public String getCommandStatus() {
        return "{ioget}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "0}";
    }

    @Override
    public String getCommandCloseRelay(int relayIndex) {
        return "{ioseto" + (relayIndex + 1) + "1}";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
        return getCommandOpenRelay(relayIndex);
    }

    @Override
    public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
        return getCommandCloseRelay(relayIndex);
    }
}
