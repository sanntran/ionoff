package net.ionoff.center.server.broker;

public class BrokerResponse {

    private Integer status;
    private String code;
    private Object data;

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }

    public boolean isInternalError() {
        return status != null && status.intValue() == 500;
    }
}
