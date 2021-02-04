package net.ionoff.csnlink.service;

import net.ionoff.csnlink.model.Link;
import net.ionoff.csnlink.model.LinkList;
import net.ionoff.csnlink.repository.LinkRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
public class LinkService {

	@Autowired
	private LinkRepository linkRepository;

    @Transactional
	public List<Link> insert(List<String> links) {
		List<Link> results = new ArrayList<>();
		for (String link : links) {
			Link entity = new Link();
			entity.setLink(link);
			results.add(insert(entity));
		}
		return results;
	}

    private Link insert(Link link) {
        Optional<Link> optional = linkRepository.findByLink(link.getLink());
        if (optional.isPresent()) {
            return optional.get();
        } else {
            Link entity = optional.get();
            entity.setStatus(Link.Status.PENDING);
            return linkRepository.save(link);
        }
    }

	@Transactional
	public Link save(Link link) {
		Optional<Link> optional = linkRepository.findByLink(link.getLink());
		if (optional.isPresent()) {
			Link entity = optional.get();
			entity.setStatus(link.getStatus());
			return linkRepository.save(entity);
		} else {
			return linkRepository.save(link);
		}
	}

    @Transactional
    public List<Link> save(List<Link> links) {
        LinkList results = new LinkList();
        for (Link link : links) {
            results.add(save(link));
        }
        return results;
    }

	@Transactional
	public List<Link> get(Link.Status status, Integer limit) {
        List<Link> links = linkRepository.findByStatus(status);
        if (limit == null || links.size() <= limit) {
            return links;
        }
        return links.subList(0, limit);
	}
}
