package net.ionoff.csnlink.model;

import javax.persistence.*;

@Entity
@Table(name = "links")
public class Link {

    public enum Status {
        PENDING, PROCESSED, ERROR
    }

    public Link() {}

    public Link(String link, Status status) {
        this.link = link;
        this.status = status;
    }

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "link")
    private String link;

    @Column(name = "status")
    @Enumerated(value = EnumType.STRING)
    private Status status;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Link{" +
                "id=" + id +
                ", link='" + link + '\'' +
                ", status=" + status +
                '}';
    }
}
