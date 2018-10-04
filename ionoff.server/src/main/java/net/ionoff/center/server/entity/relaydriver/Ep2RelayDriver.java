package net.ionoff.center.server.entity.relaydriver;

import net.ionoff.center.server.entity.RelayDriver;

public class Ep2RelayDriver extends RelayDriver {

    static {
        RelayDriver.MODELS.put(model(), Ep2RelayDriver.class.getClass());
    }

    private static String model() {
        return "HLAB_EP2";
    }

    @Override
    public int getInput() {
        return 0;
    }

    @Override
    public int getOutput() {
        return 20;
    }

    @Override
    public String getModel() {
        return model();
    }

    @Override
    public boolean autoPublish() {
        return false;
    }

    @Override
    public int getOnlineBuffer() {
        return 15000;
    }

    @Override
    public String getProtocol() {
        return "http";
    }

    @Override
    public String getCommandStatus() {
        return "http://" + getIp() + ":" + getPort() + "/AllIOS.cgi";
    }

    @Override
    public String getCommandOpenRelay(int relayIndex) {
        return null;
    }

    @Override
    public String getCommandCloseRelay(int relayIndex) {
        String path = "";
        switch (relayIndex) {
            case 0:
                path = "8?2=IO0";
                break;
            case 1:
                path = "8?3=IO1";
                break;
            case 2:
                path = "8?4=IO2";
                break;
            case 3:
                path = "8?5=IO3";
                break;
            case 4:
                path = "8?6=IO4";
                break;
            case 5:
                path = "8?7=IO5";
                break;
            case 6:
                path = "8?8=IO6";
                break;
            case 7:
                path = "8?9=IO7";
                break;
            case 8:
                path = "9?0=IO8";
                break;
            case 9:
                path = "9?1=IO9";
                break;
            case 10:
                path = "9?2=IO10";
                break;
            case 11:
                path = "9?3=IO11";
                break;
            case 12:
                path = "9?4=IO12";
                break;
            case 13:
                path = "9?5=IO13";
                break;
            case 14:
                path = "9?6=IO14";
                break;
            case 15:
                path = "9?7=IO15";
                break;
            case 16:
                path = "9?8=IO16";
                break;
            case 17:
                path = "9?9=IO17";
                break;
            case 18:
                path = "9?a=IO18";
                break;
            case 19:
                path = "9?b=IO19";
                break;
        }
        return "http://" + getIp() + ":" + getPort() + "/" + path;
    }

    @Override
    public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
        return getCommandOpenRelay(relayIndex);
    }

    @Override
    public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
        int revert = 0;
        if (autoRevert != null && autoRevert.intValue() == 1) {
            revert = 1;
        }
        if (revert == 1) {
            String path = "";
            switch (relayIndex) {
                case 0:
                    path = "6?2=IO0";
                    break;
                case 1:
                    path = "6?3=IO1";
                    break;
                case 2:
                    path = "6?4=IO2";
                    break;
                case 3:
                    path = "6?5=IO3";
                    break;
                case 4:
                    path = "6?6=IO4";
                    break;
                case 5:
                    path = "6?7=IO5";
                    break;
                case 6:
                    path = "6?8=IO6";
                    break;
                case 7:
                    path = "6?9=IO7";
                    break;
                case 8:
                    path = "7?0=IO8";
                    break;
                case 9:
                    path = "7?1=IO9";
                    break;
                case 10:
                    path = "7?2=IO10";
                    break;
                case 11:
                    path = "7?3=IO11";
                    break;
                case 12:
                    path = "7?4=IO12";
                    break;
                case 13:
                    path = "7?5=IO13";
                    break;
                case 14:
                    path = "7?6=IO14";
                    break;
                case 15:
                    path = "7?7=IO15";
                    break;
                case 16:
                    path = "7?8=IO16";
                    break;
                case 17:
                    path = "7?9=IO17";
                    break;
                case 18:
                    path = "7?a=IO18";
                    break;
                case 19:
                    path = "7?b=IO19";
                    break;
            }
            return "http://" + getIp() + ":" + getPort() + "/" + path;
        }
        return getCommandCloseRelay(relayIndex);
    }

    @SuppressWarnings("unused")
    private String getParamChangeStatus(int relayIndex) {
        String code = "";
        switch (relayIndex) {
            case 0:
                code = "2?2=IO0";
                break;
            case 1:
                code = "2?3=IO1";
                break;
            case 2:
                code = "2?4=IO2";
                break;
            case 3:
                code = "2?5=IO3";
                break;
            case 4:
                code = "2?6=IO4";
                break;
            case 5:
                code = "2?7=IO5";
                break;
            case 6:
                code = "2?8=IO6";
                break;
            case 7:
                code = "2?9=IO7";
                break;
            case 8:
                code = "3?0=IO8";
                break;
            case 9:
                code = "3?1=IO9";
                break;
            case 10:
                code = "3?2=IO10";
                break;
            case 11:
                code = "3?3=IO11";
                break;
            case 12:
                code = "3?4=IO12";
                break;
            case 13:
                code = "3?5=IO13";
                break;
            case 14:
                code = "3?6=IO14";
                break;
            case 15:
                code = "3?7=IO15";
                break;
            case 16:
                code = "3?8=IO16";
                break;
            case 17:
                code = "3?9=IO17";
                break;
            case 18:
                code = "3?a=IO18";
                break;
            case 19:
                code = "3?b=IO19";
                break;
        }
        return code;
    }

}
