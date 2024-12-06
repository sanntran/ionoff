package net.ionoff.center.server.entity.controller;

import net.ionoff.center.server.entity.Controller;

public class P8Controller extends Controller {

    static {
        Controller.MODELS.put(model(), P8Controller.class);
    }

    private static String model() {
        return "IONOFF_P8";
    }

    @Override
    public int getInput() {
        return 8;
    }

    @Override
    public int getOutput() {
        return 8;
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

	@Override
	public int getOneSecondDelay() {
		return 0;
	}
}