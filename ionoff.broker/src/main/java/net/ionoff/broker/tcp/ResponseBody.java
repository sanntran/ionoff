package net.ionoff.broker.tcp;

import java.io.Serializable;

public class ResponseBody implements Serializable {

    private int status;
    private String code;
    private Object data;

    public ResponseBody(int status, String code, Object data) {
        this.status = status;
        this.code = code;
        this.data = data;
    }

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
}
