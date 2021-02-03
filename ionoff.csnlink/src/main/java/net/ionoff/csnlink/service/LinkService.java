package net.ionoff.csnlink.service;

import net.ionoff.csnlink.model.Link;
import net.ionoff.csnlink.repository.LinkRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
public class LinkService {

	@Autowired
	private LinkRepository linkRepository;

	public List<Link> save(List<String> links) {
		List<Link> results = new ArrayList<>();
		for (String link : links) {
			Link entity = new Link();
			entity.setStatus(Link.Status.PENDING);
			entity.setLink(link);
			results.add(save(entity));
		}
		return results;
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
	public List<Link> get(Link.Status status) {
		return linkRepository.findByStatus(status);
	}
}
