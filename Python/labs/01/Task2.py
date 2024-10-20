def emails_shortener(emails):
    domain_names = {}

    for email in emails:
        username, domain = email.split("@")
        if domain not in domain_names:
            domain_names[domain] = []
        domain_names[domain].append(username)

    result = set()

    for domain, names in domain_names.items():
        if len(names) == 1:
            email = f"{names[0]}@{domain}"
        else:
            string = ",".join(names)
            email = f"{{{string}}}@{domain}"
        result.add(email)
    return result

assert emails_shortener([
    "pesho@abv.bg", 
    "gosho@abv.bg",
    "sasho@abv.bg",
]) == {
    "{pesho,gosho,sasho}@abv.bg"
}

assert emails_shortener([
    "tinko@fmi.uni-sofia.bg", 
    "minko@fmi.uni-sofia.bg", 
    "pesho@pesho.org",
]) == {
    "{tinko,minko}@fmi.uni-sofia.bg", 
    "pesho@pesho.org",
}

assert emails_shortener([
    "toi_e@pesho.org",
    "golemiq@cyb.org",
]) == {
    "toi_e@pesho.org",
    "golemiq@cyb.org",
}
