package net.ionoff.center.server.mediaplayer.model;

public class MediaPlayerResponse {

    private String status;
    private String message;
    private Object object;
    private String clazz;

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Object getObject() {
        return object;
    }

    public void setObject(Object object) {
        this.object = object;
    }

    public String getClazz() {
        return clazz;
    }

    public void setClazz(String clazz) {
        this.clazz = clazz;
    }

    public boolean isError() {
        return "error".equals(status);
    }

    public boolean isRequestError() {
        return "error".equals(status) && "BadRequestException".equals(clazz);
    }

    public boolean isInternalError() {
        return "error".equals(status) && "InternalPlayerError".equals(clazz);
    }

}
